#nullable enable
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Basics
{
    public static class Extensions
    {
        public static string JoinC(this IEnumerable<object> e, string separator = ", ")
            => string.Join(separator, e);

        [DebuggerStepThrough]
        public static IEnumerator<int> GetEnumerator(this Range r)
        {
            int ToInt(Index i) => i.IsFromEnd ? -i.Value : i.Value;
            for (int n = ToInt(r.Start); n < ToInt(r.End); n++) yield return n;
        }

        public static TValue TryGetOrAdd<TKey, TValue>(this IDictionary<TKey, TValue> d, TKey key, Func<TValue> toAdd)
        {
            if (d.TryGetValue(key, out var value))
                return value;
            else
            {
                var adding = toAdd();
                d.Add(key, adding);
                return adding;
            }
        }

        public static TValue TryGetOrAdd<TKey, TValue>(this KeyedCollection<TKey, TValue> d, TKey key, Func<TValue> toAdd)
            where TKey : notnull
        {
            if (d.TryGetValue(key, out var value))
                return value;
            else
            {
                var adding = toAdd();
                d.Add(adding);
                return adding;
            }
        }

        public static ImmutableArray<T2> ImmutSelect<T, T2>(this IEnumerable<T> e, Func<T, T2> f) => e.Select(f).ToImmutableArray();
    }
    class ListComparer<T> : IEqualityComparer<ImmutableArray<T>> where T : notnull
    {
        public bool Equals(ImmutableArray<T> x, ImmutableArray<T> y)
        {
            return x.SequenceEqual(y);
        }

        public int GetHashCode(ImmutableArray<T> obj)
        {
            int hashcode = 0;
            foreach (T t in obj)
            {
                hashcode ^= t.GetHashCode();
            }
            return hashcode;
        }
    }
    class ExceptionThrower
    {
        public static Exception E = new();
    }
    public abstract class KeyedDictionary<TKey, TValue> : Dictionary<TKey, TValue>
        where TKey : notnull
    {
        public abstract TKey GetKeyForItem(TValue val);

        public KeyedDictionary(IEqualityComparer<TKey>? comparer) : base(comparer) { }

        public void Add(TValue val) => Add(GetKeyForItem(val), val);
    }
}
#nullable disable