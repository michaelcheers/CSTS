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
    using static CS.MemberList;
    using static Program;
    using static Basics.ExceptionThrower;
    using System.Text.RegularExpressions;
    using Microsoft.CodeAnalysis.CSharp;

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
            switch (inst)
            {
                case MethodInstantiation method:
                    void WriteTrivia ()
                    {
                        WriteLine(); 
                        if (method.Symbol.IsStatic) Write("static ");
                    }
                    if (method.Symbol switch
                    {
                        IFieldSymbol => true,
                        IMethodSymbol ms when ms.MethodKind is MethodKind.PropertyGet => true,
                        _ => false
                    })
                    {
                        var (name, type) = method.Symbol switch
                        {
                            IFieldSymbol fs => (fs.Name, fs.Type),
                            IMethodSymbol { AssociatedSymbol: IPropertySymbol ps } => (ps.Name, ps.Type)
                        };
                        WriteTrivia();
                        Write(name);
                        Write(": ");
                        Write(type);
                        Write(" = ");
                        WriteDefaultValueOf(type);
                        Write(";");
                        break;
                    }
                    var (body, exprBody) = method.From.Member.Declaration switch
                    {
                        AccessorDeclarationSyntax acc => (acc.Body, acc.ExpressionBody),
                        PropertyDeclarationSyntax pds => (null, pds.ExpressionBody),
                        BaseMethodDeclarationSyntax mds => (mds.Body, mds.ExpressionBody),
                        null => (null, null)
                    };
                    if (body == null && exprBody == null) return;
                    WriteTrivia();
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
                    WriteLine();
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
                case IfStatementSyntax or WhileStatementSyntax:
                {
                    Write(statement switch { IfStatementSyntax => "if", WhileStatementSyntax => "while" });
                    Write(" (");
                    Write(statement switch
                    {
                        IfStatementSyntax ifss => ifss.Condition,
                        WhileStatementSyntax wss => wss.Condition
                    }, containing);
                    Write(")");
                    StartCodeBlock();
                    StatementSyntax body = statement switch
                    {
                        IfStatementSyntax ifss => ifss.Statement,
                        WhileStatementSyntax wss => wss.Statement
                    };
                    if (body is BlockSyntax block)
                    {
                        foreach (var subStatement in block.Statements)
                            Write(subStatement, containing);
                    }
                    else
                        Write(body, containing);
                    EndCodeBlock();
                    break;
                }
                case DoStatementSyntax doss:
                {
                    Write("do");
                    StartCodeBlock();
                    if (doss.Statement is BlockSyntax block)
                    {
                        foreach (var subStatement in block.Statements)
                            Write(subStatement, containing);
                    }
                    else
                        Write(doss.Statement, containing);
                    EndCodeBlock();
                    Write("while (");
                    Write(doss.Condition, containing);
                    Write(");");
                    break;
                }
                case LocalDeclarationStatementSyntax ldss:
                    ITypeSymbol ts = (ITypeSymbol)model.GetSymbolInfo(ldss.Declaration.Type).Symbol!;
                    WriteJoin(ldss.Declaration.Variables, varia =>
                    {
                        Write("let ");
                        Write(varia.Identifier.ValueText);
                        Write(": ");
                        Write(ts);
                        if (varia.Initializer is { } init)
                        {
                            Write(" = ");
                            Write(init.Value, containing);
                        }
                        Write(";");
                    }, "\n");
                    break;
                case ExpressionStatementSyntax ess:
                    Write(ess.Expression, containing);
                    Write(";");
                    break;
                case ReturnStatementSyntax rss:
                    Write("return");
                    if (rss.Expression is {} e)
                    {
                        Write(" ");
                        Write(e, containing);
                    }
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
                        Write(child.MemName);
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

        public static MemberInstantiation FindSymbol (ISymbol sym) => sym switch
        {
            IPropertySymbol ps => FindSymbol(ps),
            _ => FindSymbolNonProperty(sym)
        };

        static MemberInstantiation FindSymbolNonProperty (ISymbol sym) =>
            sym.OriginalDefinition != sym ?
                allSymbols[sym.ContainingType!].Children.Single(mi => mi.Symbol.Equals(sym)) :
                ((MethodInstance)memberLookup[sym].CreateInstance(allSymbols[sym.ContainingType!])).InstantiateNonGeneric();

        public static MethodInstantiation FindSymbol(IFieldSymbol sym)
            => (MethodInstantiation)FindSymbolNonProperty(sym);

        public static MethodInstantiation FindSymbol(IMethodSymbol sym)
            => (MethodInstantiation)FindSymbolNonProperty(sym);

        public static MethodInstantiation FindSymbol(IPropertySymbol sym)
            => (MethodInstantiation)FindSymbolNonProperty(sym.GetMethod!);

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
                case InvocationExpressionSyntax:
                case ElementAccessExpressionSyntax:
                case BinaryExpressionSyntax:
                    IMethodSymbol ms = expr switch
                    {
                        InvocationExpressionSyntax inv => (IMethodSymbol)model.GetSymbolInfo(inv.Expression).Symbol!,
                        ElementAccessExpressionSyntax => ((IPropertySymbol)model.GetSymbolInfo(expr).Symbol!).GetMethod!,
                        BinaryExpressionSyntax bes => (IMethodSymbol)model.GetSymbolInfo(bes).Symbol!,
                    };
                    if (expr is InvocationExpressionSyntax ies && ms == null)
                    {
                        ExpressionSyntax e = ies.Expression;
                        if (e is ParenthesizedExpressionSyntax { Expression: CastExpressionSyntax } pes)
                            e = pes.Expression;
                        Write(e, containing);
                        Write("(");
                        WriteJoin(ies.ArgumentList.Arguments, arg => Write(arg.Expression, containing));
                        Write(")");
                        break;
                    }
                    WriteInvocation(
                        FindSymbol(ms),
                        expr switch
                        {
                            _ when ms.IsStatic => null,
                            InvocationExpressionSyntax { Expression: MemberAccessExpressionSyntax maes } => maes.Expression,
                            ElementAccessExpressionSyntax eaes => eaes.Expression
                        },
                        expr switch
                        {
                            InvocationExpressionSyntax inv => inv.ArgumentList.Arguments.Select(a => a.Expression).ToArray(),
                            ElementAccessExpressionSyntax eaes => eaes.ArgumentList.Arguments.Select(a => a.Expression).ToArray(),
                            BinaryExpressionSyntax bes => new[] { bes.Left, bes.Right }
                        },
                        containing
                    );
                    break;
                case BaseObjectCreationExpressionSyntax boces:
                    WriteInvocation(
                        FindSymbol((IMethodSymbol)model.GetSymbolInfo(boces).Symbol!),
                        left: null,
                        boces.ArgumentList,
                        containing,
                        boces.Initializer
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
                case AssignmentExpressionSyntax aes when aes.Kind() == SyntaxKind.SimpleAssignmentExpression:
                    Write(aes.Left, containing);
                    Write(" = ");
                    Write(aes.Right, containing);
                    break;
                case ParenthesizedExpressionSyntax pes:
                    Write("(");
                    Write(pes.Expression, containing);
                    Write(")");
                    break;
                case CastExpressionSyntax ces when model.GetTypeInfo(ces.Type) is { Type.TypeKind: TypeKind.Delegate }:
                    Write(ces.Expression, containing);
                    break;
                case LambdaExpressionSyntax les:
                    switch (les)
                    {
                        case SimpleLambdaExpressionSyntax sles:
                            Write(sles.Parameter.Identifier.ValueText);
                            break;
                        case ParenthesizedLambdaExpressionSyntax ples:
                            Write("(");
                            WriteJoin(ples.ParameterList.Parameters, p => Write(p.Identifier.ValueText));
                            Write(")");
                            break;
                        default: throw E;
                    }
                    Write(" => ");
                    switch (les)
                    {
                        case { ExpressionBody: {} eb }:
                            Write(eb, containing);
                            break;
                        case { Body: BlockSyntax bod }:
                            Write("{");
                            indent++;
                            foreach (var statement in bod.Statements)
                                Write(statement, containing);
                            indent--;
                            WriteLine();
                            Write("}");
                            break;
                        default: throw E;
                    }
                    break;
                default:
                    throw E;
            }
        }
        public void WriteInvocation (MethodInstantiation mem, ExpressionSyntax? left, ArgumentListSyntax? argumentList, MethodInstantiation containing, InitializerExpressionSyntax? initializer = null) =>
            WriteInvocation(
                mem: mem,
                left: left,
                arguments: argumentList is {} al ? al.Arguments.Select(a => a.Expression).ToArray() : Array.Empty<ExpressionSyntax>(),
                containing: containing,
                initializer: initializer
            );

        public void WriteInvocation (MethodInstantiation mem, ExpressionSyntax? left, ExpressionSyntax[] arguments, MethodInstantiation containing, InitializerExpressionSyntax? initializer = null)
        {
            IMethodSymbol ms = (IMethodSymbol)mem.Symbol;
            string template = mem.InvocationTemplate;
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
            replacements.Add(("{...}", (Action)(() => { WriteJoin(arguments, a => Write(a, containing)); })));
            if (ms is { IsStatic: false, MethodKind: not MethodKind.Constructor })
                if (left != null)
                    replacements.Add(("{this}", () => Write(left, containing)));
                else throw E;
            foreach (var (parameter, i) in ms.Parameters.Select((v, i) => (v, i)))
            {
                replacements.Add(("{" + parameter.Name + "}", () => Write(arguments[i], containing)));
                replacements.Add(("{" + i + "}", () => Write(arguments[i], containing)));
            }
            if (initializer != null && initializer.Expressions.Any(e => e is not AssignmentExpressionSyntax))
                replacements.Add(("{...$arr}", () => WriteJoin(initializer.Expressions.Where(e => e is not AssignmentExpressionSyntax), e => Write(e, containing))));
            WriteReplacing(template, replacements);
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