#nullable enable
using Basics;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Basics.ExceptionThrower;

namespace CSharpParser_Tree.CSMembers
{
    public abstract class MemberInstance
    {
        public abstract Member Member { get; }
        public abstract ClassInstantiation? Upper { get; }
    }
    public abstract class GenericMemberInstance<InstT, Sym, ThisType> : MemberInstance
        where InstT: GenericMemberInstantiation<ThisType, Sym>
        where Sym: ISymbol
        where ThisType: GenericMemberInstance<InstT, Sym, ThisType>
    {
        public InstantiationList<InstT, ThisType, Sym> instantiations = new();

        protected abstract InstT _Instantiate(ImmutableArray<ClassInstantiation> genericArgs, Sym symbol);
        public InstT Instantiate(ImmutableArray<ClassInstantiation> genericArgs, Sym symbol)
            => instantiations.TryGetOrAdd(genericArgs, () => _Instantiate(genericArgs, symbol));
    }
    public sealed class InstantiationList<T, Inst, Sym> : KeyedCollection<ImmutableArray<ClassInstantiation>, T>
        where T: GenericMemberInstantiation<Inst, Sym>
        where Inst: MemberInstance
        where Sym : ISymbol
    {
        protected override ImmutableArray<ClassInstantiation> GetKeyForItem(T item) => item.GenericArgs;

        public InstantiationList() : base(new Basics.ListComparer<ClassInstantiation>()) { }
    }
    public abstract class MemberInstantiation
    {
        public abstract MemberInstance From { get; }
        public abstract ISymbol Symbol { get; }
    }
    public abstract class _MemInst : MemberInstantiation 
    {
        protected abstract ISymbol SymbolImpl { get; }
        public sealed override ISymbol Symbol => SymbolImpl;
    }
    public abstract class GenericMemberInstantiation<Inst, Sym> : _MemInst where Inst : MemberInstance where Sym : ISymbol
    {
        public abstract override Inst From { get; }
        protected sealed override ISymbol SymbolImpl => Symbol;
        public abstract new Sym Symbol { get; }
        public abstract ImmutableArray<ClassInstantiation> GenericArgs { get; }

        public IEnumerable<KeyValuePair<string, ClassInstantiation>> AllGenericArgs =>
            (From.Upper == null ? ImmutableArray<KeyValuePair<string, ClassInstantiation>>.Empty : From.Upper.AllGenericArgs).Concat(LocalGenericArgs);

        public IEnumerable<KeyValuePair<string, ClassInstantiation>> LocalGenericArgs => Symbol switch
        {
            INamedTypeSymbol ts => ts.TypeParameters.Zip(GenericArgs, (name, expected) =>
                new KeyValuePair<string, ClassInstantiation>(name.Name, expected)
            ),
            _ when GenericArgs.Length == 0 => ImmutableArray<KeyValuePair<string, ClassInstantiation>>.Empty
        };
    }
    public class MemberList : IReadOnlyCollection<Member>, IReadOnlyList<Member>
    {
        List<Member> _members = new();

        public Member this[int index] => _members[index];
        public Member? this[string name] => _members.FirstOrDefault(v => v.Name == name);
        public Member? this[ISymbol symbol] => _members.FirstOrDefault(v => v.Symbol == symbol);

        public int Count => _members.Count;
        public bool Contains(Member item) => _members.Contains(item);
        public void CopyTo(Member[] array, int arrayIndex) => _members.CopyTo(array, arrayIndex);
        public IEnumerator<Member> GetEnumerator() => _members.GetEnumerator();
        public int IndexOf(Member item) => _members.IndexOf(item);
        IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();

        public void Add(Member item)
        {
            memberLookup.Add(item);
            _members.Add(item);
        }

        public class MemberLookup : KeyedDictionary<ISymbol, Member>
        {
            public override ISymbol GetKeyForItem(Member val) => val.Symbol;

            public MemberLookup() : base(SymbolEqualityComparer.Default) { }
        }

        public static MemberLookup memberLookup = new();
    }
    public abstract class Member
    {
        public abstract CSharpSyntaxNode? Declaration { get; }
        public abstract ISymbol Symbol { get; }
        public abstract string Name { get; }
        public abstract Class? Upper { get; }

        protected abstract MemberInstance _CreateInstance(ClassInstantiation? upper);
        public abstract MemberInstance CreateInstance (ClassInstantiation? upper);
    }
    public static class _MemC<T, SymT>
        where T: MemberInstance
        where SymT: ISymbol
    {
        public abstract class __ : Member<T>
        {
            public sealed override ISymbol Symbol => SymbolImpl;
            public abstract SymT SymbolImpl { get; }
        }
        public abstract class _ : __
        {
            public sealed override SymT SymbolImpl => throw new NotImplementedException();
            public new abstract SymT Symbol { get; }
        }
    }
    public abstract class GenericMember<InstanceT, InstT, Sym> : _MemC<InstanceT, Sym>._
        where InstanceT : GenericMemberInstance<InstT, Sym, InstanceT>
        where InstT : GenericMemberInstantiation<InstanceT, Sym>
        where Sym: ISymbol
    {
        public abstract ImmutableArray<string> GenericArgs { get; }

        public InstT InstantiateAsNS()
            => CreateInstance(Upper?.InstantiateAsNS()).Instantiate(ImmutableArray<ClassInstantiation>.Empty, Symbol);
    }
    public abstract class Member<InstanceT> : Member where InstanceT: MemberInstance
    {
        protected abstract override InstanceT _CreateInstance(ClassInstantiation? upper);

        public sealed override InstanceT CreateInstance(ClassInstantiation? upper)
            => upper == null ? _CreateInstance(null) : upper.memberInstances.TryGetOrAdd(this, () => _CreateInstance(upper));
    }
    public class Class : GenericMember<ClassInstance, ClassInstantiation, INamespaceOrTypeSymbol>
    {
        public MemberList members = new();

        public override INamespaceOrTypeSymbol Symbol { get; } // INamespaceSymbol | IArrayTypeSymbol | INamedTypeSymbol
        public override MemberDeclarationSyntax? Declaration { get; } // TypeDeclarationSyntax | NamespaceDeclarationSyntax
        public override string Name { get; }
        public override Class? Upper { get; }

        public override ImmutableArray<string> GenericArgs => Declaration switch
        {
            TypeDeclarationSyntax {TypeParameterList: {} tpl} => tpl.Parameters.ImmutSelect(v => v.Identifier.ValueText),
            _ => ImmutableArray<string>.Empty
        };

        public Class(in INamespaceOrTypeSymbol symbol, in MemberDeclarationSyntax decl, in string name, in Class _upper)
        {
            Symbol = symbol;
            Declaration = decl;
            Name = name;
            Upper = _upper;
        }

        public override string ToString()
        {
            throw new NotImplementedException();
        }

        protected override ClassInstance _CreateInstance(ClassInstantiation? upper) => new (this, upper);
    }
    public class ClassInstance : GenericMemberInstance<ClassInstantiation, INamespaceOrTypeSymbol, ClassInstance>
    {
        public override Class Member { get; }
        public override ClassInstantiation? Upper { get; }

        public ClassInstance (in Class member, in ClassInstantiation? upper)
        {
            Member = member;
            Upper = upper;
        }

        protected override ClassInstantiation _Instantiate(ImmutableArray<ClassInstantiation> genericArgs, INamespaceOrTypeSymbol symbol)
            => new (this, genericArgs, symbol);
    }
    public class ClassInstantiation : GenericMemberInstantiation<ClassInstance, INamespaceOrTypeSymbol>
    {
        public override ClassInstance From { get; }
        public override INamespaceOrTypeSymbol Symbol { get; }
        public override ImmutableArray<ClassInstantiation> GenericArgs { get; }

        public class MemberInstanceLookup
        {
            Dictionary<Member, MemberInstance> _dic = new();

            public void Add<InstT>(Member<InstT> k, InstT v) where InstT : MemberInstance
            {
                _dic.Add(k, v);
            }

            public InstT Get<InstT>(Member<InstT> mem) where InstT : MemberInstance => (InstT)_dic[mem];
            public InstT TryGetOrAdd<InstT>(Member<InstT> mem, Func<InstT> toAdd) where InstT : MemberInstance
                => (InstT)_dic.TryGetOrAdd(mem, toAdd);
        }

        public MemberInstanceLookup memberInstances = new();

        public ClassInstantiation
            (in ClassInstance from, in ImmutableArray<ClassInstantiation> genericArgs, in INamespaceOrTypeSymbol symbol)
        {
            From = from;
            GenericArgs = genericArgs;
            Symbol = symbol;
        }

        public override string ToString()
        {
            var generics = LocalGenericArgs.Select(v => v.Value.ToString()).JoinC();
            var upperS = From.Upper?.ToString();
            return (string.IsNullOrEmpty(upperS) ? "" : (upperS + ".")) + From.Member.Name + (generics == string.Empty ? "" : ("<" + generics + ">"));
        }
    }
    public class Method : GenericMember<MethodInstance, MethodInstantiation, ISymbol>
    {
        public override ISymbol Symbol { get; } // IFieldSymbol | IMethodSymbol
        public override string Name { get; }
        public override Class Upper { get; }
        public override CSharpSyntaxNode Declaration { get; } // AccessorDeclarationSyntax | MethodDeclarationSyntax | PropertyDeclarationSyntax

        public MethodBody Body => Declaration switch
        {
            AccessorDeclarationSyntax ads => new MethodBody { Body = ads.Body, ExpressionBody = ads.ExpressionBody },
            MethodDeclarationSyntax mds => new MethodBody { Body = mds.Body, ExpressionBody = mds.ExpressionBody },
            PropertyDeclarationSyntax pds => new MethodBody { ExpressionBody = pds.ExpressionBody },
            FieldDeclarationSyntax fds => new MethodBody()
        };

        public AnnotatedTypeSymbol ReturnType => Symbol switch
        {
            IFieldSymbol fs => new AnnotatedTypeSymbol { TypeSymbol = fs.Type, RefKind = RefKind.Ref },
            IMethodSymbol { ReturnsByRef: false } ms => new AnnotatedTypeSymbol { TypeSymbol = ms.ReturnType },
            IMethodSymbol { ReturnsByRef: true } ms => new AnnotatedTypeSymbol
            {
                TypeSymbol = ms.ReturnType,
                RefKind = RefKind.Ref
            },
        };

        public ImmutableArray<Parameter> Parameters => (Symbol switch
        {
            IMethodSymbol ms => ms.Parameters.Select(v => new Parameter
            {
                Name = v.Name,
                Type =
                {
                    TypeSymbol = v.Type,
                    RefKind = v.RefKind
                }
            }),
            IFieldSymbol fs => ImmutableArray<Parameter>.Empty
        }).ToImmutableArray();

        public override ImmutableArray<string> GenericArgs => Symbol switch
        {
            IMethodSymbol ms => ms.TypeParameters.ImmutSelect(v => v.Name),
            IFieldSymbol => ImmutableArray<string>.Empty
        };

        protected override MethodInstance _CreateInstance(ClassInstantiation? upper)
            => new (this, upper ?? throw E);

        public Method(in ISymbol symbol, in CSharpSyntaxNode decl, in string name, in Class _upper)
        {
            Symbol = symbol;
            Declaration = decl;
            Name = name;
            Upper = _upper;
        }

        public override string ToString()
        {
            throw new NotImplementedException();
        }
    }
    public class MethodInstance : GenericMemberInstance<MethodInstantiation, ISymbol, MethodInstance>
    {
        public override Method Member { get; }
        public override ClassInstantiation Upper { get; }

        protected override MethodInstantiation _Instantiate(ImmutableArray<ClassInstantiation> genericArgs, ISymbol symbol)
            => new (this, genericArgs, symbol);

        public MethodInstance(in Method member, in ClassInstantiation upper)
        {
            Member = member;
            Upper = upper;
        }
    }
    public class MethodInstantiation : GenericMemberInstantiation<MethodInstance, ISymbol>
    {
        public override MethodInstance From { get; }
        public override ISymbol Symbol { get; }
        public override ImmutableArray<ClassInstantiation> GenericArgs { get; }

        public MethodInstantiation(in MethodInstance from, in ImmutableArray<ClassInstantiation> genericArgs, in ISymbol symbol)
        {
            From = from;
            GenericArgs = genericArgs;
            Symbol = symbol;
        }
    }
    public struct Parameter
    {
        public AnnotatedTypeSymbol Type;
        public string Name;
    }
    public struct AnnotatedTypeSymbol
    {
        public ITypeSymbol TypeSymbol;
        public RefKind RefKind;
    }
    public struct MethodBody
    {
        public ArrowExpressionClauseSyntax? ExpressionBody;
        public BlockSyntax? Body;
    }
    public abstract class CSExplorer<T>
    {
        public abstract T ExploreClass(Class c);
        public abstract T ExploreMethod(Method m);
        public T ExploreMember(Member m) => m switch
        {
            Class c => ExploreClass(c),
            Method met => ExploreMethod(met)
        };
    }
}
#nullable disable