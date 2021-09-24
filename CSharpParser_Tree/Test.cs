using System;
using CStoTS.Attributes;

Program.Main();

namespace CStoTS
{
    namespace Attributes
    {
        [External]
        public class TemplateAttribute : Attribute
        {
            public TemplateAttribute(string template) { }
        }
        [External]
        public class InvocationTemplateAttribute : Attribute
        {
            public InvocationTemplateAttribute(string template) { }
        }
        [External]
        public class ExternalAttribute : Attribute
        {

        }
    }
}

namespace System
{
    [Template("void"), External]   public struct Void { }
    [Template("number"), External]
    public struct Int32
    {
        [InvocationTemplate("{a} > {b}"), External]
        public static bool operator > (int a, int b) => a > b;
    }
    [Template("boolean"), External] public struct Boolean { }
    [Template("string"), External] public struct String {  }
    public class Attribute { }
}

[External]
public static class Console
{
    [Template("console.log")]
    //[Template("(v => [new Text(v), document.createElement('br')].forEach(e => document.body.appendChild(e)))")]
    public static extern void WriteLine(int value);
    [Template("console.log")]
    public static extern void WriteLine(string value);
}

[External]
[Template("{T}[]")]
class List<T>
{
    [InvocationTemplate("[{...$arr}]")]
    public extern List();

    [Template("{this}.push")]
    public extern void Add(T item);

    public extern int Count { [Template("{this}.length")] get; }

    [InvocationTemplate("{this}[{this}.length - 1]")]
    public extern T Last();

    public extern T this[int index] { [InvocationTemplate("{this}[{index}]")] get; }
}

public class Program
{
    public static void Main()
    {
        List<string> theBest = new() { "Hey", "You" };
        theBest.Add("Buffoon");
        if (theBest.Count > 0)
        {
            Console.WriteLine(theBest.Last());
        }
        int cool;
    }
}