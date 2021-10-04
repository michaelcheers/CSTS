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
        [InvocationTemplate("{a} === {b}")]
        public static bool operator ==(int a, int b) => a == b;
        [InvocationTemplate("{a} !== {b}")]
        public static bool operator !=(int a, int b) => a != b;
        [InvocationTemplate("{a} > {b}")]
        public static bool operator > (int a, int b) => a > b;
        [InvocationTemplate("{a} >= {b}")]
        public static bool operator >=(int a, int b) => a >= b;
        [InvocationTemplate("{a} < {b}")]
        public static bool operator <(int a, int b) => a < b;
        [InvocationTemplate("{a} <= {b}")]
        public static bool operator <=(int a, int b) => a <= b;
        [InvocationTemplate("{a} + {b}")]
        public static int operator + (int a, int b) => a + b;
        [InvocationTemplate("{a} - {b}")]
        public static int operator -(int a, int b) => a - b;
        [InvocationTemplate("{a} * {b}")]
        public static int operator *(int a, int b) => a * b;
        [InvocationTemplate("{a} / {b}")]
        public static int operator /(int a, int b) => a / b;
    }
    [Template("boolean"), External] public struct Boolean { }
    [Template("string"), External] public struct String
    {
        [Template("{this}.length")]
        public extern int Length { get; }
    }
    public class Attribute { }
    public delegate TResult Func<out TResult>();
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

public class Vector2
{
    public int X { get; set; }
}

public class Program
{
    public static int BITSHIFT(int a, int b)
    {
        while (b-- >= 0) a *= 2;
        return a;
    }
    public static void Main()
    {
        int x = 0;
        Console.WriteLine(x);
        do
        {
            Console.WriteLine(BITSHIFT(1, x));
        }
        while (x++ < 32);
    }
}