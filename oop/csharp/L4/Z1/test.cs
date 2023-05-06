using System;

public class SlowaFibonacciego
{
    public static string GenerateWord(int n)
    {
        if (n <= 0)
        {
            throw new ArgumentException("n must be greater than 0");
        }
        else if (n == 1)
        {
            return "b";
        }
        else if (n == 2)
        {
            return "a";
        }
        else
        {
            return GenerateWord(n - 1) + GenerateWord(n - 2);
        }
    }

    static void Main(string[] args)
    {
        Console.WriteLine("Enter the value of n: ");
        int n = int.Parse(Console.ReadLine());

        try
        {
            string s = GenerateWord(n);
            Console.WriteLine("S{0} = {1}", n, s);
        }
        catch (ArgumentException ex)
        {
            Console.WriteLine(ex.Message);
        }

        Console.ReadKey();
    }
}
