﻿using Microsoft.CodeAnalysis;
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
            AssignmentExpressionSyntax aes when aes.Left == node => aes.Kind() == SyntaxKind.SimpleAssignmentExpression ? ps.SetMethod! : throw E,
            _ => ps.GetMethod!
        };

        public override void DefaultVisit(SyntaxNode node)
        {
            if (node is TypeSyntax or ElementAccessExpressionSyntax or MemberAccessExpressionSyntax or BaseObjectCreationExpressionSyntax)
            {
                switch (model.GetSymbolInfo(node).Symbol)
                {
                    case ITypeSymbol tsym:
                        AddGenericArgs(tsym);
                        break;
                    case IFieldSymbol fsym:
                        ((Method)memberLookup[fsym.OriginalDefinition]).CreateInstance(AddGenericArgs(fsym.ContainingType)).InstantiateNonGeneric();
                        break;
                    case IMethodSymbol tsm:
                        ClassInstantiation upper = AddGenericArgs(tsm.ContainingType);
                        Method m = (Method)memberLookup[tsm.OriginalDefinition];
                        IEnumerable<ClassInstantiation> typeArgs = tsm.TypeArguments.Select(AddGenericArgs);
                        ImmutableDictionary<string, ClassInstantiation> newGenericVals =
                            upper.AllGenericArgs.Concat(typeArgs.Zip(tsm.TypeParameters.Select(v => v.Name), (type, name) =>
                                new KeyValuePair<string, ClassInstantiation>(name, type)
                            )).ToImmutableDictionary();
                        new SecondPass(newGenericVals).Visit(m.Declaration);
                        m.CreateInstance(upper).Instantiate(typeArgs.ToImmutableArray(), tsm);
                        break;
                    case IPropertySymbol ps:
                        IMethodSymbol mSym = GetAccessedProp(node, ps);
                        ((Method)memberLookup[mSym.OriginalDefinition]).CreateInstance(AddGenericArgs(mSym.ContainingType)).InstantiateNonGeneric();
                        break;
                }
            }
            base.DefaultVisit(node);
        }
    }
}
