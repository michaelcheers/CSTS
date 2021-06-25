Test.TwoDictionaries<A, B>();
Test.TwoDictionaries<B, A>();

class A { } class B { }

class Test
{
    public static void TwoDictionaries<K, V>()
    {
        Dictionary<K, K> d1;
        Dictionary<K, V> d2;
        Dictionary<V, V> d3;
    }
}
class Dictionary<K, V>
{

}