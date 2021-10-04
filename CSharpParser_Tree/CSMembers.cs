#pragma warning disable CS8846 // The switch expression does not handle all possible values of its input type (it is not exhaustive).
#pragma warning disable CS8509 // The switch expression does not handle all possible values of its input type (it is not exhaustive).
#pragma warning disable RS1024 // Compare symbols correctly
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
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Basics.ExceptionThrower;

namespace CSharpParser_Tree.CS
{
    using static Program;

    public abstract class MemberInstance
    {
        public abstract Member Member { get; }
        public abstract ClassInstantiation? Upper { get; }
        public abstract IEnumerable<MemberInstantiation> Instantiations { get; }
    }
    public abstract class GenericMemberInstance<InstT, Sym, ThisType> : MemberInstance
        where InstT: GenericMemberInstantiation<ThisType, Sym>
        where Sym: ISymbol
        where ThisType: GenericMemberInstance<InstT, Sym, ThisType>
    {
        public override InstantiationList<InstT, ThisType, Sym> Instantiations { get; } = new();

        protected abstract InstT _Instantiate(ImmutableArray<ClassInstantiation> genericArgs, Sym symbol);

        public InstT Instantiate(ImmutableArray<ClassInstantiation> genericArgs, Sym symbol)
            => Instantiations.TryGetOrAdd(genericArgs, () => _Instantiate(genericArgs, symbol));
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

        public virtual bool IsStatic => From.Member.Symbol is { IsStatic: true };

        public virtual string MemName => From.Member.Name;

        public virtual string Template =>
            TemplateAttribute != null ?
                ((string)TemplateAttribute.ConstructorArguments[0].Value!) :
                IsStatic ?
                    $"{From.Upper!.ToString()}.{MemName}" :
                    "{this}." + MemName;

        public virtual bool IsExternal => From.Member.Symbol.GetAttributes().Any(v => v.AttributeClass == ExternalAttribute);

        public AttributeData? TemplateAttribute => From.Member.Symbol.FindAttribute(Program.TemplateAttribute);
    }
    public abstract class _MemInst : MemberInstantiation 
    {
        protected abstract ISymbol SymbolImpl { get; }
        public sealed override ISymbol Symbol => SymbolImpl;
    }
    public abstract class GenericMemberInstantiation<Inst, Sym> : _MemInst
        where Inst : MemberInstance
        where Sym : ISymbol
    {
        public abstract override Inst From { get; }
        protected sealed override ISymbol SymbolImpl => Symbol;
        public abstract new Sym Symbol { get; }
        public abstract ImmutableArray<ClassInstantiation> GenericArgs { get; }

        public IEnumerable<KeyValuePair<string, ClassInstantiation>> AllGenericArgs =>
            (From.Upper is {} u ? u.AllGenericArgs : ImmutableArray<KeyValuePair<string, ClassInstantiation>>.Empty).Concat(LocalGenericArgs);

        public IEnumerable<KeyValuePair<string, ClassInstantiation>> LocalGenericArgs => Symbol switch
        {
            INamedTypeSymbol ts => ts.TypeParameters.Zip(GenericArgs, (name, expected) =>
                new KeyValuePair<string, ClassInstantiation>(name.Name, expected)
            ),
            IMethodSymbol ms => ms.TypeParameters.Zip(GenericArgs, (name, expected) =>
                new KeyValuePair<string, ClassInstantiation>(name.Name, expected)
            ),
            _ when GenericArgs.Length == 0 => ImmutableArray<KeyValuePair<string, ClassInstantiation>>.Empty
        };

        public ClassInstantiation SubWithAllGenericArgs (ITypeSymbol toSub)
            => new SecondPass(AllGenericArgs.ToImmutableDictionary()).AddGenericArgs(toSub);

        public ClassInstantiation SubWithAllGenericArgs(TypeSyntax toSub)
            => SubWithAllGenericArgs((ITypeSymbol?)model.GetSymbolInfo(toSub).Symbol ?? throw E);
    }
    public class MemberList : IReadOnlyCollection<Member>, IReadOnlyList<Member>
    {
        [DebuggerBrowsable(DebuggerBrowsableState.RootHidden)]
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
            public override ISymbol GetKeyForItem(Member val) =>
                val is Method { Declaration: OperatorDeclarationSyntax { ExpressionBody: {} bod } } m &&
                m.Symbol.FindAttribute(InvocationTemplateAttribute) != null &&
                bod.DescendantNodes().OfType<BinaryExpressionSyntax>().SingleOrDefault() is {} bes ?
                    model.GetSymbolInfo(bes).Symbol! : val.Symbol!;

            public MemberLookup() : base(SymbolEqualityComparer.Default) { }
        }

        public static MemberLookup memberLookup = new();

        public override string ToString() => _members.JoinC();
    }
    public abstract class Member
    {
        public abstract CSharpSyntaxNode? Declaration { get; }
        public abstract ISymbol Symbol { get; }
        public abstract string Name { get; }
        public abstract Class? Upper { get; }

        public abstract MemberInstance CreateInstance (ClassInstantiation upper);
    }
    public abstract class Member<InstanceT> : Member where InstanceT : MemberInstance
    {
        protected abstract InstanceT _CreateInstance(ClassInstantiation? upper);

        public sealed override InstanceT CreateInstance(ClassInstantiation? upper)
            => upper != null ? upper.memberInstances.TryGetOrAdd(this, () => _CreateInstance(upper)) : (InstanceT)(MemberInstance)top.From;
    }
    public static class _MemC<InstanceT, SymT>
        where InstanceT: MemberInstance
        where SymT: ISymbol
    {
        public abstract class __ : Member<InstanceT>
        {
            public sealed override ISymbol Symbol => SymbolImpl;
            protected abstract SymT SymbolImpl { get; }
        }
        public abstract class _ : __
        {
            protected sealed override SymT SymbolImpl => Symbol;
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
    public class Class : GenericMember<ClassInstance, ClassInstantiation, INamespaceOrTypeSymbol>
    {
        public MemberList members = new();

        public override INamespaceOrTypeSymbol Symbol { get; } // INamespaceSymbol | IArrayTypeSymbol | INamedTypeSymbol
        // See https://github.com/dotnet/runtime/issues/54719
        public override /*MemberDeclarationSyntax*/CSharpSyntaxNode? Declaration { get; } // TypeDeclarationSyntax | NamespaceDeclarationSyntax
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

        public static ClassInstantiation CreateRoot (Class c)
        {
            var instC = c._CreateInstance(null);
            var inst = instC.Instantiate(ImmutableArray<ClassInstantiation>.Empty, c.Symbol);
            //inst.memberInstances.Add(c, instC);
            return inst;
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

        public override bool IsStatic => true; // different than static vs non-static class

        public override bool IsExternal => Symbol.IsNamespace ? Children.All(c => c.IsExternal) : base.IsExternal;

        public class MemberInstanceLookup
        {
            [DebuggerBrowsable(DebuggerBrowsableState.RootHidden)]
            Dictionary<Member, MemberInstance> _dic = new();

            public void Add<InstT>(Member<InstT> k, InstT v) where InstT : MemberInstance
            {
                _dic.Add(k, v);
            }

            public IEnumerable<MemberInstance> Values => _dic.Values;
            public InstT Get<InstT>(Member<InstT> mem) where InstT : MemberInstance => (InstT)_dic[mem];
            [DebuggerStepThrough]
            public InstT TryGetOrAdd<InstT>(Member<InstT> mem, Func<InstT> toAdd) where InstT : MemberInstance
                => (InstT)_dic.TryGetOrAdd(mem, toAdd);
        }

        [DebuggerBrowsable(DebuggerBrowsableState.RootHidden)]
        public MemberInstanceLookup memberInstances = new();

        public ClassInstantiation
            (in ClassInstance from, in ImmutableArray<ClassInstantiation> genericArgs, in INamespaceOrTypeSymbol symbol)
        {
            From = from;
            GenericArgs = genericArgs;
            Symbol = symbol;
        }

        public IEnumerable<MemberInstantiation> Children
            => (/*From.Upper == null ? memberInstances.Values.Where(v => v != this.From) : */memberInstances.Values).SelectMany(v => v.Instantiations);

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
        public override CSharpSyntaxNode? Declaration { get; } // AccessorDeclarationSyntax | MethodDeclarationSyntax | PropertyDeclarationSyntax
        public bool IsField => Symbol is IFieldSymbol || (Declaration is PropertyDeclarationSyntax decl && decl.AccessorList is {} accL
            && accL.Accessors.SingleOrDefault(v => v.Modifiers.Any(SyntaxKind.GetAccessorDeclaration)) is {} getAcc &&
            getAcc.Body == null && getAcc.ExpressionBody == null
        );

        public override ImmutableArray<string> GenericArgs => Symbol switch
        {
            IMethodSymbol ms => ms.TypeParameters.ImmutSelect(v => v.Name),
            IFieldSymbol => ImmutableArray<string>.Empty
        };

        protected override MethodInstance _CreateInstance(ClassInstantiation? upper)
            => new (this, upper ?? throw E);

        public Method(in ISymbol symbol, in CSharpSyntaxNode? decl, in string name, in Class _upper)
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

        public MethodInstantiation InstantiateNonGeneric()
            => Instantiate(ImmutableArray<ClassInstantiation>.Empty, Upper.Symbol.GetMembers().Single(s => s.OriginalDefinition == Member.Symbol));
    }
    public record Parameter(string name, ClassInstantiation type);
    public class MethodInstantiation : GenericMemberInstantiation<MethodInstance, ISymbol>
    {
        public override MethodInstance From { get; }
        public override ISymbol Symbol { get; }
        public override ImmutableArray<ClassInstantiation> GenericArgs { get; }

        public override string Template => Symbol switch
        {
            _ when TemplateAttribute != null => base.Template,
            IMethodSymbol { IsExtensionMethod: true } => "{this}" + $"[{MemName}]",
            IMethodSymbol { MethodKind: MethodKind.Constructor } => $"new {From.Upper}",
            _ => base.Template
        };

        public AttributeData? InvocationTemplateAttribute =>
            From.Member.Symbol.GetAttributes().Where(v => v.AttributeClass == Program.InvocationTemplateAttribute).SingleOrDefault();

        public string InvocationTemplate =>
            Symbol is not IMethodSymbol ? throw E :
                InvocationTemplateAttribute != null ?
                    (string)InvocationTemplateAttribute.ConstructorArguments[0].Value! :
                    Template + "({...})";

        //public struct _Stage2
        //{
        //    public List<CSExpr> Body;
        //    public ClassInstantiation ReturnType;
        //}

        //_Stage2? _s2;

        //public _Stage2 Stage2
        //{
        //    get
        //    {
        //        if (_s2 is {} val)
        //            return val;
        //        switch (From.Member.Declaration)
        //        {
        //            case MethodDeclarationSyntax mds:
        //                _Stage2 result = default;
        //                result.ReturnType = SubWithAllGenericArgs(mds.ReturnType);
        //                switch (mds)
        //                {
        //                    case { ExpressionBody: { } exprBody }:
        //                        break;
        //                    case { Body: { } blockBody }:
        //                        InterpretExpr(blockBody);
        //                        break;
        //                    default: throw new NotImplementedException();
        //                }
        //                return result;
        //            default: throw new NotImplementedException();
        //        }
        //    }
        //}

        //public IEnumerable<CSExpr> InterpretExpr(StatementSyntax statement)
        //{

        //    yield break;
        //}


        //public MethodBody Body => From.Declaration switch
        //{
        //    AccessorDeclarationSyntax ads => new MethodBody { Body = ads.Body, ExpressionBody = ads.ExpressionBody },
        //    MethodDeclarationSyntax mds => new MethodBody { Body = mds.Body, ExpressionBody = mds.ExpressionBody },
        //    PropertyDeclarationSyntax pds => new MethodBody { ExpressionBody = pds.ExpressionBody },
        //    FieldDeclarationSyntax fds => new MethodBody()
        //};

        public ClassInstantiation ReturnType => Symbol switch
        {
            IFieldSymbol fs => allSymbols[fs.Type],
            IMethodSymbol { ReturnsByRef: false } ms => allSymbols[ms.ReturnType],
            IMethodSymbol { ReturnsByRef: true } ms => throw new NotImplementedException(),
        };

        //public ImmutableArray<Parameter> Parameters => Symbol switch
        //{
        //    IMethodSymbol ms => ms.Parameters.ImmutSelect(v => new Parameter(v.Name, allSymbols[v.Type])),
        //    IFieldSymbol fs => ImmutableArray<Parameter>.Empty
        //};

        public MethodInstantiation(in MethodInstance from, in ImmutableArray<ClassInstantiation> genericArgs, in ISymbol symbol)
        {
            From = from;
            GenericArgs = genericArgs;
            Symbol = symbol;
        }

        public override string ToString()
        {
            var generics = LocalGenericArgs.Select(v => v.Value.ToString()).JoinC();
            if (generics != string.Empty) generics = $"<{generics}>";
            return "static " + From.Member.Name + generics + "()";
        }
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
    [VoidVersion]
    public abstract partial class CSExplorer<T>
    {
        public abstract T ExploreClassInstantiation(ClassInstantiation c);
        public abstract T ExploreMethodInstantiation(MethodInstantiation m);
    }
    public abstract partial class CSExplorer<T>
    {
        public T ExploreInstantiation(MemberInstantiation m) => m switch
        {
            ClassInstantiation c => ExploreClassInstantiation(c),
            MethodInstantiation met => ExploreMethodInstantiation(met)
        };
        public IEnumerable<T> Explore(Class top)
        {
            yield return ExploreClassInstantiation(top.InstantiateAsNS());
        }
    }
    //public abstract partial class CSExplorer
    //{
    //    public void ExploreInstantiation(MemberInstantiation m)
    //    {
    //        switch (m)
    //        {
    //            case ClassInstantiation c   : ExploreClassInstantiation(c);   break;
    //            case MethodInstantiation met: ExploreMethodInstantiation(met);break;
    //        };
    //    }
    //    public void Explore(Class top)
    //    {
    //        ExploreClassInstantiation(top.InstantiateAsNS());
    //    }
    //}
}
#nullable disable