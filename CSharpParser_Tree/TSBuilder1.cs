using System;
using System.Collections.Generic;
using System.Linq;

namespace CSharpParser_Tree
{
    using CS;
    using TSMembers;

    public static partial class Program
    {
        public static partial void ThirdPass()
        {
            var tp = new TSBuilder1();
            var newTop = tp.Explore(top.From.Member);
        }
    }

    public class TSBuilder1 : CSExplorer<TSMember>
    {
        public override TSClass ExploreClassInstantiation(ClassInstantiation cs)
        {
            var ts = new TSClass { Name = cs.From.Member.Name };
            foreach (var csinst in cs.Children)
                ts.members.Add(ExploreInstantiation(csinst));
            return ts;
        }
        public override TSMethod ExploreMethodInstantiation(MethodInstantiation m)
        {
            var ts = new TSMethod { Name = m.From.Member.Name };
            return ts;
        }
    }
}
