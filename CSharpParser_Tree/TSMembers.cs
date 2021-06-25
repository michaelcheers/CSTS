using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

#nullable disable

namespace CSharpParser_Tree.TSMembers
{
    public abstract class TSMember
    {

    }
    public abstract class TSExpr
    {

    }
    public class TSClass : TSMember
    {
        public List<TSMember> members = new();
        public string Name;
    }
    public class TSMethod : TSMember
    {
        public List<TSMember> members = new();
        public string Name;
        public ImmutableArray<TSClass> Parameters;
    }
    public class ClassAccessExpression : TSExpr
    {
        public TSClass Left;
        public TSMember Right;
    }
    public class MemberAccessExpression : TSExpr
    {
        public TSExpr Left;
        public TSMember Right;
    }
    public class InvocationExpression : TSExpr
    {
        public TSExpr ToInvoke;
        public ImmutableArray<TSExpr> Arguments;
    }
}

#nullable enable