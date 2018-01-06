public class arrayhomework
{
    public static void main (String[] args) 
    {
 

        String [ ]words = new String[size]  ;                    //declare and allocate an array

        
    
        numArray[0] = 1;                    //store 1 in first element    
        numArray[1] = 3;     
        numArray[2] = 5;  
        numArray[3] = 7;   
        numArray[4] = 9;

        

        for (int i=0; i < words.length; i++)     //loop as long as the array
        {
            System.out.print(words[i] + " ");    //print element value
        }
    
        System.out.println();                       //print empty line


    //------------ declare and allocate space ---------------------------

        float[ ] num2Array = new float[5];          //declare and allocate space
    
        num2Array[0] = 1.90f;                       //store 1 in first element    
        num2Array[1] = 2.89f;     
        num2Array[2] = 3.14f;  
        num2Array[3] = 4.66f;   
        num2Array[4] = 5.31f;

        for (int i=0; i < num2Array.length; i++)    //loop as long as the array
        {
            System.out.print(num2Array[i] + " ");   //print element value
        }

        System.out.println();                       //print empty line


    //------------ declare, allocate and initialize -----------------------

        char[ ] vowels = {'A','E','I','O','U'};     //declare, allocate and init
    
        for (int i=0; i < vowels.length; i++)       //loop as long as the array
        {
            System.out.print(vowels[i] + " ");      //print element value
        }

        System.out.println();                       //print empty line
    }
}
