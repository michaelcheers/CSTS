using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharpParser_Tree
{
    using static Program;
    using Basics;
    using CS;

    public static partial class Program
    {
        public static CSharpSyntaxTree tree;
        public static CompilationUnitSyntax root;
        public static CSharpCompilation compilation;
        public static SemanticModel model;

        public static Dictionary<ITypeSymbol, ClassInstantiation> allSymbols = new(SymbolEqualityComparer.Default);
        public static Dictionary<ClassInstantiation, SpecialType> primitiveTypes = new();
        public static INamedTypeSymbol TemplateAttribute, InvocationTemplateAttribute, ExternalAttribute;
        public static ClassInstantiation top;
        public static ClassInstance ArrayType;

        static Program()
        {
            tree = (CSharpSyntaxTree)CSharpSyntaxTree.ParseText(File.ReadAllText("Test.cs"));
            tree = ZeroethPass(tree);
            root = tree.GetCompilationUnitRoot();
            compilation = CSharpCompilation.Create("HelloWorld").AddSyntaxTrees(tree);
            model = compilation.GetSemanticModel(tree);
            ExternalAttribute = compilation.GetTypeByMetadataName($"CStoTS.Attributes.{nameof(ExternalAttribute)}")!;
            TemplateAttribute = compilation.GetTypeByMetadataName($"CStoTS.Attributes.{nameof(TemplateAttribute)}")!;
            InvocationTemplateAttribute = compilation.GetTypeByMetadataName($"CStoTS.Attributes.{nameof(InvocationTemplateAttribute)}")!;
            top = Class.CreateRoot(new Class(model.Compilation.GlobalNamespace, null!, "", null!));
            ArrayType = new Class(null!, null!/*Will be fixed!*/, "Array", top.From.Member).CreateInstance(top);
        }

        #region Old Code
        public static string PrintExpr(StatementSyntax expr)
        {
            if (expr is ExpressionStatementSyntax ess)
            {
                return PrintExpr(ess.Expression);
            }
            return "";
        }
        public static string PrintExpr(ExpressionSyntax expr)
        {
            StringBuilder r = new();
            void Body()
            {
                if (expr is LiteralExpressionSyntax les)
                {
                    r.Append(les.Token.ValueText); return;
                }
                var symbolInfo = model.GetSymbolInfo(expr);
                var symbol = symbolInfo.Symbol ?? symbolInfo.CandidateSymbols.FirstOrDefault();
                if (expr is InvocationExpressionSyntax ies)
                {
                    r.Append(PrintExpr(ies.Expression));
                    if (symbol is IMethodSymbol ms)
                        r.Append(ms.TypeArguments switch
                        {
                            var args when args.Any() => "<" + args.JoinC() + ">",
                            var args => ""
                        });
                    r.Append('(');
                    r.Append(ies.ArgumentList.Arguments.Select(v => PrintExpr(v.Expression)).JoinC());
                    r.Append(')');
                }
                else if (expr is MemberAccessExpressionSyntax maes)
                {
                    r.Append(maes.Expression);
                    r.Append('.');
                    r.Append(maes.Name);
                }
                else if (expr is IdentifierNameSyntax ins)
                {
                    r.Append(ins.Identifier.ValueText);
                }
                else if (expr is PredefinedTypeSyntax pts)
                {
                    r.Append(pts.Keyword.ValueText);
                }
                else if (expr is CastExpressionSyntax ces)
                {
                    r.Append('(');
                    r.Append('<');
                    r.Append(PrintExpr(ces.Type));
                    r.Append('>');
                    r.Append(PrintExpr(ces.Expression));
                    r.Append(')');
                }
                else if (expr is LambdaExpressionSyntax lambda)
                {
                    r.Append('(');
                    var parameters = model.GetSymbolInfo(lambda).Symbol;
                    if (parameters is IMethodSymbol ms)
                        r.Append(ms.Parameters.Select(v => v.Name + ':' + v.Type).JoinC());
                    r.Append(") => ");
                    r.Append(PrintExpr(lambda.ExpressionBody!));
                }
                else
                    r.Append(expr.ToString());
            }
            Body();
            return r.ToString();
        }
        public static int indent = 0;
        public static void Indent()
        {
            foreach (var _ in 0..indent) Console.Write("    ");
        }
        public static void WriteLine(object o)
        {
            Indent();
            Console.WriteLine(o);
        }
        public static void PrintMembers(IEnumerable<MemberDeclarationSyntax> mems)
        {
            WriteLine("{");
            indent++;
            foreach (var mem in mems)
                PrintMember(mem);
            indent--;
            WriteLine("}");
        }
        public static void PrintMember(MemberDeclarationSyntax mem)
        {
            if (mem is ClassDeclarationSyntax cds)
            {
                WriteLine(string.Join(" ",
                    "class",
                    cds.Identifier.ValueText +
                    (cds.TypeParameterList is not { } tpl ? null :
                        ("<" + tpl.Parameters.Select(v => v.Identifier.ValueText).JoinC() + ">"))
                ));
                PrintMembers(cds.Members);
            }
            else if (mem is GlobalStatementSyntax gss)
            {
                WriteLine(PrintExpr(gss.Statement));
            }
            else if (mem is MethodDeclarationSyntax mds)
            {
                WriteLine(string.Join(" ",
                    mds.Modifiers.Any(SyntaxKind.StaticKeyword) ? "static" : null,
                    mds.Identifier.ValueText +
                    (mds.TypeParameterList is not { } tpl ? null :
                        ("<" + tpl.Parameters.Select(v => v.Identifier.ValueText).JoinC() + ">")),
                    "(" + mds.ParameterList.Parameters.Select(v => v.Identifier.ValueText + ":" + PrintExpr(v.Type!)).JoinC() + "):",
                    PrintExpr(mds.ReturnType),
                    "{"
                    ));
                var body = mds.Body;
                indent++;
                if (body == null)
                {
                    var eBody = mds.ExpressionBody!;
                    WriteLine("return " + PrintExpr(eBody.Expression) + "");
                }
                else
                {
                    foreach (var statement in body.Statements)
                    {
                        WriteLine(PrintExpr(statement) + ";");
                    }
                }
                indent--;
                WriteLine("}");
            }
        }
        #endregion

        public static partial CSharpSyntaxTree ZeroethPass(CSharpSyntaxTree tree);
        public static partial void FirstPass();
        public static partial void SecondPass();
        public static partial void OutputJS();
        public static void PrintNew (MemberInstantiation member)
        {
            switch (member)
            {
                case ClassInstantiation ci:
                    WriteLine($"class {ci} {{");
                    indent++;
                    foreach (var mem in ci.Children)
                        PrintNew(mem);
                    indent--;
                    WriteLine("}");
                    break;
                case MethodInstantiation mi:
                    WriteLine(mi);
                    break;
            }
        }
        public static void Main()
        {
            FirstPass();
            SecondPass();
            OutputJS();
            //foreach (var c in top)
            //{
            //}

            //foreach (var mem in top.Children)
            //{
            //    PrintNew(mem);
            //}

            // Old:
            /*foreach (var mem in root.Members)
                PrintMember(mem);*/
        }
        public static AttributeData? FindAttribute (this ISymbol symbol, INamedTypeSymbol attr) =>
            symbol.GetAttributes().SingleOrDefault(v => v.AttributeClass == attr);
    }
}
