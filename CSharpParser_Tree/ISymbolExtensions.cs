using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharpParser_Tree
{
    public static class ISymbolExtensions
    {
        public static ImmutableArray<ISymbol> ExplicitOrImplicitInterfaceImplementations(this ISymbol symbol)
        {
            if (symbol.Kind != SymbolKind.Method && symbol.Kind != SymbolKind.Property && symbol.Kind != SymbolKind.Event)
                return ImmutableArray<ISymbol>.Empty;

            var containingType = symbol.ContainingType;
            var query = from iface in containingType.AllInterfaces
                        from interfaceMember in iface.GetMembers()
                        let impl = containingType.FindImplementationForInterfaceMember(interfaceMember)
                        where symbol.Equals(impl)
                        select interfaceMember;
            return query.ToImmutableArray();
        }
        public static PropertyDeclarationSyntax GetProperty (this AccessorDeclarationSyntax ads)
            => (PropertyDeclarationSyntax)ads!.Parent!.Parent!;
    }
}
