using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;

namespace CSharpParser_Tree
{
    using Basics;
    using CSharpParser_Tree.CS;
    using Microsoft.CodeAnalysis;
    using Microsoft.CodeAnalysis.CSharp.Syntax;
    using static Program;
    using static Basics.ExceptionThrower;
    using System.Text.RegularExpressions;

    public static partial class Program
    {
        public static partial void OutputJS()
        {
            var writer = new JsWriter(Console.Out);
            writer.WriteChildren(top);
            writer.WriteLine();
            writer.Write("Program.Main();");
            writer.WriteLine();
        }
    }

    public record JsWriter (TextWriter Out)
    {
        public int indent = 0;

        public void WriteChildren (ClassInstantiation ci)
        {
            foreach (var child in ci.Children)
                Write(child);
        }

        public void Write (MemberInstantiation inst)
        {
            if (inst.IsExternal) return;
            WriteLine();
            switch (inst)
            {
                case MethodInstantiation method:
                    if (method.Symbol is IFieldSymbol fs)
                    {
                        Write(fs.Name);
                        Write(": ");
                        Write(fs.Type);
                        Write(" = ");
                        WriteDefaultValueOf(fs.Type);
                        Write(";");
                        break;
                    }
                    var (body, exprBody) = method.From.Member.Declaration switch
                    {
                        AccessorDeclarationSyntax acc => (acc.Body, acc.ExpressionBody),
                        PropertyDeclarationSyntax pds => (null, pds.ExpressionBody),
                        BaseMethodDeclarationSyntax mds => (mds.Body, mds.ExpressionBody)
                    };
                    if (body == null && exprBody == null) return;
                    if (method.Symbol.IsStatic)
                        Write("static ");
                    if (method.Symbol.Kind == SymbolKind.Property &&
                        method.From.Member.Declaration is AccessorDeclarationSyntax { Body: null, ExpressionBody: null } ads)
                    {
                        var pds = (PropertyDeclarationSyntax)ads.Parent!.Parent!;
                        Write(pds.Identifier.ValueText);
                    }
                    else
                    {
                        if (method.From.Member.Declaration is ConstructorDeclarationSyntax)
                            Write("constructor");
                        else
                            Write(method.From.Member.Name);
                        switch (method.From.Member.Symbol)
                        {
                            case IMethodSymbol ms:
                                Write('(');
                                WriteJoin(ms.Parameters, p =>
                                {
                                    Write(p.Name);
                                    Write(": ");
                                    Write(p.Type);
                                });
                                Write(')');
                                break;
                        }
                    }
                    if (method.From.Member.Declaration is not ConstructorDeclarationSyntax)
                    {
                        Write(": ");
                        Write(method.ReturnType);
                    }
                    if ((body != null) == (exprBody != null)) throw E;
                    if (body != null)
                    {
                        StartCodeBlock();
                        foreach (var s in body.Statements) Write(s, method);
                        EndCodeBlock();
                    }
                    else if (exprBody != null)
                    {
                    }
                    break;
                case ClassInstantiation ci:
                    Write("class ");
                    Write(ci.From.Member.Name);
                    StartCodeBlock();
                    WriteChildren(ci);
                    EndCodeBlock();
                    break;
            }
        }

        public void StartCodeBlock ()
        {
            Write(" {");
            indent++;
        }

        public void EndCodeBlock()
        {
            indent--;
            WriteLine();
            Write("}");
        }

        public void Write (string s) => Out.Write(s);
        public void Write (char   s) => Out.Write(s);
        public void Write(ClassInstantiation s)
        {
            AttributeData? nameAttr = s.From.Member.Symbol.GetAttributes().Where(v => TemplateAttribute.Equals(v.AttributeClass)).SingleOrDefault();
            string? specialName = nameAttr is { } nn ? (string)nn.ConstructorArguments.First().Value! : null;
            if (specialName == null)
            {
                Write(s.ToString());
                return;
            }
            WriteReplacing(specialName, s.AllGenericArgs.Select(kvp => ("{" + kvp.Key + "}", (Action)(() => Write(kvp.Value)))));
        }
        public void WriteLine()
        {
            Out.WriteLine();
            if (indent > 0)
                Out.Write(new string(' ', indent * 4));
        }

        public void Write (StatementSyntax statement, MethodInstantiation containing)
        {
            WriteLine();
            switch (statement)
            {
                case BlockSyntax bs:
                    Write("{");
                    foreach (var s in bs.Statements)
                        Write(s, containing);
                    EndCodeBlock();
                    break;
                case LocalDeclarationStatementSyntax ldss:
                    ITypeSymbol ts = (ITypeSymbol)model.GetSymbolInfo(ldss.Declaration.Type).Symbol!;
                    WriteJoin(ldss.Declaration.Variables, varia =>
                    {
                        Write("let ");
                        Write(varia.Identifier.ValueText);
                        Write(": ");
                        Write(ts);
                        Write(" = ");
                        if (varia.Initializer is { } init)
                            Write(init.Value, containing);
                        else
                            WriteDefaultValueOf(allSymbols[ts]);
                        Write(";");
                    }, "\n");
                    break;
                case ExpressionStatementSyntax ess:
                    Write(ess.Expression, containing);
                    Write(";");
                    break;
                default:
                    throw E;
            }
        }

        public void WriteDefaultValueOf (ClassInstantiation ci)
            => WriteDefaultValueOf((ITypeSymbol)ci.Symbol);

        public void WriteDefaultValueOf (ITypeSymbol ts)
        {
            switch (ts.TypeKind)
            {
                case TypeKind.Array:
                case TypeKind.Class:
                case TypeKind.Interface:
                    Write("null");
                    break;
                case TypeKind.Enum:
                    Write("0");
                    break;
                case TypeKind.Struct:
                    var ci = allSymbols[ts];
                    if (primitiveTypes.TryGetValue(ci, out var primitiveT)) switch (primitiveT)
                    {
                        case SpecialType.System_Int16:
                        case SpecialType.System_Int32:
                            Write("0");
                            return;
                        case SpecialType.System_Char:
                            Write(@"'\0'");
                            return;
                        default: throw E;
                    }
                    Write("{");
                    indent++;
                    bool first = true;
                    foreach (var child in ci.Children.OfType<MethodInstantiation>())
                    {
                        if (!first) Write(",");
                        first = false;
                        WriteLine();
                        Write(child.Name);
                        Write(": ");
                        if (child.From.Member.IsField) WriteDefaultValueOf(child.ReturnType);
                    }
                    indent--;
                    WriteLine();
                    Write("}");
                    break;
                default:
                    throw E;
            }
        }

        public void WriteBody (ArrowExpressionClauseSyntax aecs, MethodInstantiation containing)
        {
            StartCodeBlock();
            WriteLine();
            Write("return ");
            Write(aecs.Expression, containing);
            Write(';');
            EndCodeBlock();
        }

        public static MemberInstantiation FindSymbol (ISymbol sym) =>
            allSymbols[sym.ContainingType!].Children.Single(mi => mi.Symbol.Equals(sym switch
                { IPropertySymbol ps => ps.GetMethod, _ => sym }
            ));

        public static MethodInstantiation FindSymbol(IFieldSymbol sym)
            => (MethodInstantiation)FindSymbol((ISymbol)sym);

        public static MethodInstantiation FindSymbol(IMethodSymbol sym)
            => (MethodInstantiation)FindSymbol((ISymbol)sym);

        public static MethodInstantiation FindSymbol(IPropertySymbol sym)
            => (MethodInstantiation)FindSymbol((ISymbol)sym);

        public void Write (ExpressionSyntax expr, MethodInstantiation containing)
        {
            var symbol = model.GetSymbolInfo(expr);
            switch (expr)
            {
                case LiteralExpressionSyntax les:
                    switch (les.Token.Value)
                    {
                        case int:
                        case float:
                        case double:
                            Write(les.Token.ValueText);
                            break;
                        case string:
                            Write("\"" + (string)les.Token.Value + "\"");
                            break;
                        default: throw E;
                    }
                    break;
                case InterpolatedStringExpressionSyntax ises:
                    Write("`");
                    foreach (var t in ises.Contents)
                    {
                        switch (t)
                        {
                            case InterpolatedStringTextSyntax ists:
                                Write(ists.TextToken.Text);
                                break;
                            case InterpolationSyntax interpSyntax:
                                Write("${");
                                Write(interpSyntax.Expression, containing);
                                Write("}");
                                break;
                            default: throw E;
                        }
                    }
                    Write("`");
                    break;
                case MemberAccessExpressionSyntax maes:
                    MemberInstantiation mi = FindSymbol(model.GetSymbolInfo(maes).Symbol!);
                    if (mi.Template.Count("{this}") > 1)
                    {
                        Write("(v => ");
                        Write(mi.Template.Replace("{this}", "v"));
                        Write(")(");
                        Write(maes.Expression, containing);
                        Write(")");
                    }
                    else
                    {
                        WriteReplacing(mi.Template, "{this}", () => Write(maes.Expression, containing));
                    }
                    break;
                case InvocationExpressionSyntax or ElementAccessExpressionSyntax:
                    var ies = new
                    {
                        Left = expr switch
                        {
                            InvocationExpressionSyntax { Expression: MemberAccessExpressionSyntax maes } => maes.Expression,
                            ElementAccessExpressionSyntax eaes => eaes.Expression
                        },
                        ArgumentList = (BaseArgumentListSyntax)
                        (
                            expr switch
                            {
                                InvocationExpressionSyntax inv => inv.ArgumentList,
                                ElementAccessExpressionSyntax eaes => eaes.ArgumentList
                            }
                        )
                    };
                    IMethodSymbol ms = expr switch
                    {
                        InvocationExpressionSyntax inv => (IMethodSymbol)model.GetSymbolInfo(inv.Expression).Symbol!,
                        ElementAccessExpressionSyntax => ((IPropertySymbol)model.GetSymbolInfo(expr).Symbol!).GetMethod!,
                    };
                    //if (model.GetSymbolInfo(left).Symbol! is not IMethodSymbol ms)
                    //{
                    //    Write(left, containing);
                    //    Write("(");
                    //    WriteJoin(ies.ArgumentList.Arguments, arg => Write(arg.Expression, containing));
                    //    Write(")");
                    //    break;
                    //}
                    string template = FindSymbol(ms).InvocationTemplate;
                    (string key, int count)[] formatStrings = Regex.Matches(template, "{[0-9a-zA-Z]+}").GroupBy(m => m.Value).Select(g => (g.Key, g.Count())).ToArray();
                    List<string> formatParams = new();
                    foreach ((string key, _) in formatStrings.Where(f => f.count > 1))
                    {
                        template = template.Replace(key, "$" + formatParams.Count);
                        formatParams.Add(key);
                    }
                    if (formatParams.Count > 0)
                    {
                        string formatVars = formatParams.Select((_, i) => "$" + i).JoinC();
                        if (formatParams.Count > 1)
                            formatVars = "(" + formatVars + ")";
                        template = $"({formatVars} => {template})({formatParams.JoinC()})";
                    }
                    List<(string, Action)> replacements = new();
                    if (!ms.IsStatic)
                        if (ies.Left is { } left)
                            replacements.Add(("{this}", () => Write(left, containing)));
                        else throw E;
                    foreach (var (parameter, i) in ms.Parameters.Select((v, i) => (v, i)))
                    {
                        replacements.Add(("{" + parameter.Name + "}", () => WriteJoin(ies.ArgumentList.Arguments, arg => Write(arg.Expression, containing))));
                        replacements.Add(("{" + i + "}", () => WriteJoin(ies.ArgumentList.Arguments, arg => Write(arg.Expression, containing))));
                    }
                    replacements.Add(("{...}", () => WriteJoin(ies.ArgumentList.Arguments, arg => Write(arg.Expression, containing))));
                    WriteReplacing(template, replacements);
                    break;
                case BaseObjectCreationExpressionSyntax boces:
                    var constructorSym = (IMethodSymbol)model.GetSymbolInfo(boces).Symbol!;
                    var constructorMem = (MethodInstantiation)allSymbols[constructorSym.ContainingType!].Children.Single(mi => mi.Symbol.Equals(constructorSym));
                    WriteReplacing(constructorMem.InvocationTemplate,
                        "{...$arr}", () =>
                        {
                            if (boces.Initializer is {} init)
                                WriteJoin(init.Expressions.Where(e => e is not AssignmentExpressionSyntax), e => Write(e, containing));
                        },
                        "{...}", () =>
                        {
                            if (boces.ArgumentList is { } al)
                                WriteJoin(al.Arguments, arg => Write(arg.Expression, containing));
                        }
                    );
                    break;
                case NameSyntax ns when ns is QualifiedNameSyntax or IdentifierNameSyntax:
                    var SYM = model.GetSymbolInfo(expr).Symbol!;
                    if (SYM is IParameterSymbol or ILocalSymbol) { Write(SYM.Name); break; }
                    var mem = allSymbols[SYM.ContainingType!].Children.Single(mi => mi.Symbol.Equals(SYM));
                    if (mem is ClassInstantiation CI) { Write(CI); break; }
                    WriteReplacing(mem.Template, "{this}", () =>
                    {
                        if (ns is QualifiedNameSyntax qns)
                            Write(qns.Left, containing);
                        else if (mem.IsStatic)
                            Write(mem.From.Upper!);
                        else if (mem.From.Upper == containing.From.Upper)
                            Write("this");
                        else
                            throw E;
                    });
                    break;
                case AssignmentExpressionSyntax aes:
                    Write(aes.Left, containing);
                    Write(" = ");
                    Write(aes.Right, containing);
                    break;
                default:
                    throw E;
            }
        }

        public void WriteInvocation (MethodInstantiation mi, ArgumentListSyntax? argumentList, MethodInstantiation containing, Action? thisK = null, InitializerExpressionSyntax? initializer = null)
        {
            List<(string, Action)> replacements = new()
            {
                ("{...}", (Action)(() => { if (argumentList is { } al) WriteJoin(al.Arguments, a => Write(a.Expression, containing)); } ))
            };
            if (initializer is {} i && i.Expressions.Any(e => e is not AssignmentExpressionSyntax))
                replacements.Add(("{...$arr}", () => WriteJoin(i.Expressions.Where(e => e is not AssignmentExpressionSyntax), e => Write(e, containing))));
            if (thisK != null) replacements.Add(("{this}", thisK));
            WriteReplacing(mi.Template, replacements);
        }

        public void Write(ITypeSymbol ts) => Write(allSymbols[ts]);

        public void WriteReplacing(string str, IEnumerable<(int idx, int length, Action writeInstead)> toReplace)
        {
            int currentIdx = 0;
            foreach (var (idx, length, writeInstead) in toReplace.OrderBy(tr => tr.idx))
            {
                Write(str.Substring(currentIdx, idx - currentIdx));
                writeInstead();
                currentIdx = idx + length;
            }
            Write(str.Substring(currentIdx));
        }

        public void WriteReplacing(string str, params (string substr, Action writeInstead)[] replacements)
           => WriteReplacing(str, (IEnumerable<(string substr, Action writeInstead)>)replacements);

        public void WriteReplacing(string str, IEnumerable<(string substr, Action writeInstead)> replacements, StringComparison strComp = StringComparison.CurrentCulture)
            => WriteReplacing(str, replacements.SelectMany(t => str.IDXs(t.substr, strComp).Select(idx => (idx, t.substr.Length, t.writeInstead))));

        public void WriteReplacing(string str, string substr, Action writeInstead, StringComparison strComp = StringComparison.CurrentCulture)
            => WriteReplacing(str, new[] { (substr, writeInstead) }, strComp);

        public void WriteReplacing(string str, string substr, Action writeInstead, string substr2, Action writeInstead2, StringComparison strComp = StringComparison.CurrentCulture)
            => WriteReplacing(str, new[] { (substr, writeInstead), (substr2, writeInstead2) }, strComp);

        public void WriteJoin<T>(IEnumerable<T> action, Action<T> run, string separator = ", ")
        {
            using (var enumer = action.GetEnumerator())
            {
                if (enumer.MoveNext())
                    run(enumer.Current);
                while (enumer.MoveNext())
                {
                    bool first = true;
                    foreach (var line in separator.Split("\n"))
                    {
                        if (!first) WriteLine();
                        Write(line);
                        first = false;
                    }
                    run(enumer.Current);
                }
            }
        }
    }
}