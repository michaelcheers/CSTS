using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharpParser_Tree
{
    using CSMembers;

    public static partial class Program
    {
        public static partial void FirstPass()
        {
            foreach (var mem in root.Members)
                FirstPass(mem, top);
        }
        public static void FirstPass(MemberDeclarationSyntax mem, Class upper)
        {
            if (mem.AttributeLists.Any(l => l.Attributes.Any(v => (v.Name switch
            {
                SimpleNameSyntax ins => ins,
                QualifiedNameSyntax qns => qns.Right,
                _ => null
            })?.Identifier.ValueText == "Ignore"))) return;

            switch (mem)
            {
                case NamespaceDeclarationSyntax:
                case TypeDeclarationSyntax:
                    var sym = (INamespaceOrTypeSymbol)model.GetDeclaredSymbol(mem)!;
                    if (upper.Symbol != sym.ContainingSymbol) throw new NotImplementedException();
                    var c = new Class(sym, mem, sym.Name, upper);
                    foreach (var m in mem switch
                    {
                        NamespaceDeclarationSyntax nds => nds.Members,
                        TypeDeclarationSyntax tds => tds.Members
                    })
                        FirstPass(m, c);
                    upper.members.Add(c);
                    break;
                case BaseMethodDeclarationSyntax bmds:
                    var symbol = model.GetDeclaredSymbol(bmds)!;
                    upper.members.Add(new Method(symbol, mem, symbol.Name, upper));
                    break;
                case PropertyDeclarationSyntax pds:
                    IPropertySymbol ps = model.GetDeclaredSymbol(pds)!;
                    if (ps.GetMethod is { } gm)
                    {
                        CSharpSyntaxNode decl = pds.AccessorList is { } accessorList ?
                            accessorList.Accessors.Single(v => v.IsKind(SyntaxKind.GetAccessorDeclaration))
                            : pds;
                        upper.members.Add(new Method(gm, decl, "get " + ps.Name, upper));
                    }
                    if (ps.SetMethod is { } sm)
                    {
                        var decl = pds.AccessorList!.Accessors.Single(v => v.IsKind(SyntaxKind.SetAccessorDeclaration));
                        upper.members.Add(new Method(sm, decl, "set " + ps.Name, upper));
                    }
                    break;
                case FieldDeclarationSyntax fds:
                    foreach (var variable in fds.Declaration.Variables)
                    {
                        IFieldSymbol fs = (IFieldSymbol)model.GetDeclaredSymbol(variable)!;
                        upper.members.Add(new Method(fs, variable, fs.Name, upper));
                    }
                    break;
                default:
                    throw new NotImplementedException();
            }
        }
    }
}
