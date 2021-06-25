namespace CSharpParser_Tree
{
    using CSMembers;
    using TSMembers;

    public class ThirdPass : CSExplorer<TSMember>
    {
        public override TSMember ExploreClass(Class c)
        {
            throw new System.NotImplementedException();
        }
        public override TSMember ExploreMethod(Method m)
        {
            throw new System.NotImplementedException();
        }
    }
}
