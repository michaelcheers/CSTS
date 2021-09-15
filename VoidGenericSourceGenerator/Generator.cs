using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Xml;

namespace VoidGenericSourceGenerator
{
    [Generator]
    public class Generator : ISourceGenerator
    {
        public void Execute(GeneratorExecutionContext context)
        {
            var voidVersionAttribute = context.Compilation.GetTypeByMetadataName("VoidVersionAttribute");
            Dictionary<INamedTypeSymbol, int> occurences = new();
            foreach (var (model, cSyntax, cSymbol, attrSyntax, attrSymbol) in 
                from tree in context.Compilation.SyntaxTrees
                let model = context.Compilation.GetSemanticModel(tree)
                from node in tree.GetRoot().DescendantNodes()
                let cSyntax = node as ClassDeclarationSyntax
                where cSyntax != null
                let cSymbol = model.GetDeclaredSymbol(cSyntax)
                let tuple = (
                    from attrSyntax in cSyntax.AttributeLists.SelectMany(v => v.Attributes)
                    from attrSymbol in cSymbol.GetAttributes()
                    where (AttributeSyntax)attrSymbol.ApplicationSyntaxReference.GetSyntax() == attrSyntax
                    where attrSymbol.AttributeClass == voidVersionAttribute
                    select (attrSyntax, attrSymbol)
                ).SingleOrDefault()
                where tuple != default
                select (model, cSyntax, cSymbol, tuple.attrSyntax, tuple.attrSymbol)
            )
            {
                ITypeParameterSymbol typeParam = attrSymbol.ConstructorArguments switch
                {
                    { Length: 0 } args => cSymbol.TypeParameters.Single(),
                    { Length: 1 } args when args[0].Value is string name => cSymbol.TypeParameters.Single(v => v.Name == name)
                };
                occurences[cSymbol] = occurences.TryGetValue(cSymbol, out int i) ? ++i : 0;
                var replacer = new VoidReplacer(model, typeParam);
                var resultC = (ClassDeclarationSyntax)replacer.Visit(cSyntax);
                var parame = resultC.TypeParameterList.Parameters;
                parame = parame.RemoveAt(parame.IndexOf(v => v.Identifier.ValueText == typeParam.Name));
                resultC = resultC.WithTypeParameterList(parame.Count == 0 ? null : resultC.TypeParameterList.WithParameters(parame));
                resultC = resultC.RemoveNode(attrSyntax, SyntaxRemoveOptions.KeepNoTrivia);
                context.AddSource(
                    cSymbol.MetadataName.Replace('`', '-') + i switch {0 => "", _ => "--" + (i + 1) },
                    Wrap(cSymbol, resultC, cSyntax).NormalizeWhitespace().ToFullString()
                );
                i++;
            }
        }

        public delegate void AddMembers (params MemberDeclarationSyntax[] mems);

        public CompilationUnitSyntax Wrap (INamedTypeSymbol type, ClassDeclarationSyntax newClass, ClassDeclarationSyntax originalDecl)
        {
            CSharpSyntaxNode declSearching = originalDecl;
            List<CSharpSyntaxNode> containingNodes = new();
            while (true)
            {
                declSearching = (CSharpSyntaxNode)declSearching.Parent;
                if (declSearching is CompilationUnitSyntax) break;
                containingNodes.Add(declSearching);
            }
            var globalNamespace = (CompilationUnitSyntax)declSearching;
            MemberDeclarationSyntax bottom = newClass;
            foreach (var node in containingNodes)
            {
                bottom = node switch
                {
                    ClassDeclarationSyntax cds => cds.WithMembers(new(bottom)),
                    NamespaceDeclarationSyntax nds => nds.WithMembers(new(bottom))
                };
            }
            return globalNamespace.WithMembers(new(bottom));
        }

        public void Initialize(GeneratorInitializationContext context)
        {
//#if DEBUG
//            if (!Debugger.IsAttached)
//            {
//                Debugger.Launch();
//            }
//#endif 
        }
    }

    public class VoidReplacer : CSharpSyntaxRewriter
    {
        public VoidReplacer (in SemanticModel semanticModel, in ITypeParameterSymbol toReplace)
        {
            this.semanticModel = semanticModel;
            this.toReplace = toReplace;
        }
        public SemanticModel semanticModel;
        public ITypeParameterSymbol toReplace;

        public override SyntaxNode VisitIdentifierName(IdentifierNameSyntax node)
        {
            if (semanticModel.GetSymbolInfo(node).Symbol == toReplace)
            {
                return SyntaxFactory.ParseTypeName("void");
            }
            else
                return base.VisitIdentifierName(node);
        }
    }
}
