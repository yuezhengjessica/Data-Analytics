/******************************************************************************
 * Converting an array to a String, and then printing
 * Using the Arrays.toString() method
 ******************************************************************************/
import java.util.*;                 //import utility class

public class arrayPrint
{
    public static void main (String[] args) 
    {   
//      int[ ]    stuff = {4, 3, 10, 12, 9, 17, 15};
        double[ ] stuff = {4, 20.3, 6.5, 8, 60.57, 45, 3};
//      char[ ]   stuff = {'f','l','b','g','m','a'};    
//      String[ ] stuff = {"Sam", "Steve", "Bill", "john", "Andy", "???"};  

        System.out.println("Printing without doing anything: ");
        System.out.println(stuff);
        System.out.println();

        System.out.println("Using Arrays.toString(): ");
        String s = Arrays.toString(stuff);
        System.out.println(s);
        System.out.println();

                
        System.out.println("Using a 'for' loop:");
        for (int i=0; i < stuff.length; i++)        
            System.out.print(stuff[i] + " ");           //print each element
        System.out.println();
        System.out.println();

        System.out.println("Using the enhanced 'for' loop:");
        for (double element : stuff)        
            System.out.print(element + " ");            //print each element
        System.out.println();
        System.out.println();

    }
}
