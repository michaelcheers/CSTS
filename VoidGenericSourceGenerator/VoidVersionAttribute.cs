using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

[AttributeUsage(AttributeTargets.Class, Inherited = false, AllowMultiple = false)]
public sealed class VoidVersionAttribute : Attribute
{
    public VoidVersionAttribute () { }
    public VoidVersionAttribute (string typeParameter) { }
}