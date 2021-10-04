using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace CSharpParser_Tree
{
    using static Basics.ExceptionThrower;
    using static Program;
    using static CS.MemberList;
    using CS;
    using Basics;

    public static partial class Program
    {
        public static partial void SecondPass()
        {
            var secondPass = new SecondPass();
            foreach (var g in root.ChildNodes().OfType<GlobalStatementSyntax>())
            {
                g.Accept(secondPass);
            }
        }
    }

    public class SecondPass : CSharpSyntaxWalker
    {
        public SecondPass() : base (SyntaxWalkerDepth.Node)
            => genericValues = ImmutableDictionary<string, ClassInstantiation>.Empty;
        public SecondPass(ImmutableDictionary<string, ClassInstantiation> gt) : base(SyntaxWalkerDepth.Node)
            => genericValues = gt;

        public ImmutableDictionary<string, ClassInstantiation> genericValues;

        public ClassInstantiation AddGenericArgs (INamedTypeSymbol sym)
        {
            if (!sym.IsGenericType) return allSymbols.TryGetOrAdd(sym, () => ((Class)memberLookup[sym]).InstantiateAsNS());
            ClassInstantiation upper = null!;
            if (sym.ContainingSymbol is { } upperSymbol)
            {
                //https://stackoverflow.com/questions/68042641/how-do-i-substitute-type-parameters-into-type-consistently
                if (upperSymbol is INamedTypeSymbol nts && nts.IsGenericType) throw new NotImplementedException();
                upper = AddGenericArgs((INamespaceOrTypeSymbol)upperSymbol);
            }
            var genericArgs = sym.TypeArguments.ImmutSelect(AddGenericArgs);
            var symWithGenerics = sym.ConstructedFrom.Construct(genericArgs.ImmutSelect(a => (ITypeSymbol)a.Symbol), sym.TypeArgumentNullableAnnotations);
            return allSymbols.TryGetOrAdd(symWithGenerics, () =>
                ((Class)memberLookup[sym.OriginalDefinition]).CreateInstance(upper).Instantiate(genericArgs, symWithGenerics)
            );
        }

        public ClassInstantiation AddGenericArgs(IArrayTypeSymbol arr)
        {
            var elemType = AddGenericArgs(arr.ElementType);
            var symWithGenerics = model.Compilation.CreateArrayTypeSymbol(
                    elementType: (ITypeSymbol)elemType.Symbol,
                    rank: arr.Rank,
                    elementNullableAnnotation: arr.ElementNullableAnnotation
                );
            return allSymbols.TryGetOrAdd(symWithGenerics, () =>
                ArrayType.Instantiate(
                    genericArgs: ImmutableArray.Create(elemType),
                    symbol: symWithGenerics
                )
            );
        }

        public ClassInstantiation AddGenericArgs(INamespaceOrTypeSymbol sym) => sym switch
        {
            INamespaceSymbol{IsGlobalNamespace:true } => top,
            INamespaceSymbol ns => ((Class)memberLookup[ns]).InstantiateAsNS(),
            ITypeParameterSymbol tps => genericValues[tps.Name],
            IArrayTypeSymbol arr => AddGenericArgs(arr),
            INamedTypeSymbol named => AddGenericArgs(named)
        };

        public static IMethodSymbol GetAccessedProp(SyntaxNode node, IPropertySymbol ps) => node.Parent switch
        {
            AssignmentExpressionSyntax aes when aes.Left == node =>
                aes.Kind() switch
                {
                    SyntaxKind.SimpleAssignmentExpression => ps.SetMethod!
                },
            _ => ps.GetMethod!
        };

        public override void DefaultVisit(SyntaxNode node)
        {
            if (node is OperatorDeclarationSyntax && model.GetDeclaredSymbol(node)?.FindAttribute(Program.InvocationTemplateAttribute) != null) return;
            if (node is TypeSyntax or ElementAccessExpressionSyntax or MemberAccessExpressionSyntax or BaseObjectCreationExpressionSyntax or BinaryExpressionSyntax)
            {
                switch (model.GetSymbolInfo(node).Symbol)
                {
                    case ITypeSymbol tsym:
                        if (tsym.TypeKind == TypeKind.Delegate) break;
                        AddGenericArgs(tsym);
                        break;
                    case ISymbol sym when sym is IFieldSymbol or IMethodSymbol or IPropertySymbol:
                        if (sym is IPropertySymbol ps) sym = GetAccessedProp(node, ps);
                        ClassInstantiation upper = AddGenericArgs(sym.ContainingType);
                        Method m = (Method)memberLookup[sym.OriginalDefinition];
                        var genericArgs = upper.AllGenericArgs;
                        var typeArgs = ImmutableArray<ClassInstantiation>.Empty;
                        if (sym is IMethodSymbol ms)
                        {
                            typeArgs = ms.TypeArguments.ImmutSelect(AddGenericArgs);
                            genericArgs = genericArgs.Concat(typeArgs.Zip
                            (
                                ms.TypeParameters.Select(v => v.Name),
                                (type, name) => new KeyValuePair<string, ClassInstantiation>(name, type)
                            ));
                        }
                        new SecondPass(genericArgs.ToImmutableDictionary()).Visit(m.Declaration);
                        m.CreateInstance(upper).Instantiate(typeArgs.ToImmutableArray(), sym.OriginalDefinition == m.Symbol.OriginalDefinition ? sym : m.Symbol);
                        break;
                }
            }
            base.DefaultVisit(node);
        }
    }
}
