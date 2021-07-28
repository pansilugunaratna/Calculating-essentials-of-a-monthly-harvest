//Harvest Statistics taken during a month. Program to get the maximum, minimum, average and median of each week with the day

import java.util.Scanner;

object HarvestWeek {
    //function to display elements
    def display(amt: Array[Array[Int]],col:Int,row:Int) : Unit =
        {
            print("\n\t");

            for (j<-0 until col){
                print(" |  Day" + (j+1));
            }
            print("\n_________________________________________________________________________________________________\n");
            for(i<-0 until row; j<-0 until col)
            {
                    if(j==0)
                        print("\nWEEK 0" + (i+1) );               
                print("\t | " + amt(i)(j));   
            }  
        }
    //function to get position of a n element
    def getPos(amt: Array[Array[Int]],col:Int,row:Int,value:Int,week:Int): Unit =
        {
            for(i<-0 until row){
                if(i == week-1)
                {
                    for(j<-0 until col)
                    {
                        if(amt(week-1)(j) == value)
                            print(" : WEEK " + week + " DAY " + (j+1));
                    }
                }
            }
        }
    //function to find maximum of each week
    def getMax(amt: Array[Array[Int]],col:Int,row:Int) : Unit =
        {
            var max = 0; 
            for(i<-0 until row; j<-0 until col)
            {
                if(j == 0)
                    max = 0;
                
                if(amt(i)(j) > max)
                    max = amt(i)(j);

                if(j==6)
                    {
                        print("\nWEEK " + (i+1) + " : ");
                        print(" Maximum = " + max);
                        print(" kg"); 
                        getPos(amt,col,row,max,(i+1));
                    }              
            }
        }
    //function to find minimum of each week
    def getMin(amt: Array[Array[Int]],col:Int,row:Int) : Unit =
        {
            var min = 100;
            for(i<-0 to row-1; j<-0 until col)
            {
                if(j == 0)
                    min = 100;               
                
                if(amt(i)(j) < min)
                    min = amt(i)(j);
                    
                if(j==6)
                    {
                        print("\nWEEK " + (i+1) + " : ");
                        print(" Minimum = " + min);
                        print(" kg"); 
                        getPos(amt,col,row,min,(i+1));
                    }             
            }
        }
    //function to find average of each week
    def getAvg(amt: Array[Array[Int]],col:Int,row:Int) : Unit =
        {
            var avg = 0;
            var sum = 0;
            for(i<-0 until row; j<-0 until col)
            {
                sum += amt(i)(j); 
                if(j==6)
                    {
                        avg = sum / 7;
                        print("\nWEEK " + (i+1) + " : ");
                        print(" Average = " + avg + " kg per day");
                        sum = 0;
                    }       
            }
        }
    //function to find median of each week
    def getMedian(amt: Array[Array[Int]],col:Int,row:Int) : Unit =
    {  
        val sorted_amt = Array.ofDim[Int](row,col);
        for(i<-0 until row; j<-0 until col)
        {
            sorted_amt(i)(j) = amt(i)(j);
        }
        // loop for rows of matrix
        for (i <- 0 until row) {
        // loop for column of matrix
        for (j <- 0 until col) {
            // loop for comparison and swapping
            for(k <- (j+1) until col){
                if(sorted_amt(i)(j) > sorted_amt(i)(k))
                {
                    // swapping of elements
                    var temp = sorted_amt(i)(j);
                    sorted_amt(i)(j) = sorted_amt(i)(k);
                    sorted_amt(i)(k) = temp;
                }
            }       
        }
        }
        for (i <- 0 until row) 
        {
            for (j <- 0 until col)
            {}  
            var median_val = (col+1)/2;
            print("\nWEEK " + (i+1) + " : ");
            print(" Median = " + sorted_amt(i)(median_val-1));
            // getPos(amt,col,row,sorted_amt(i)(median_val-1),(i+1));
        }
    }
    
    def main(args: Array[String]): Unit = {
        var col = 7;
        var row = 4;

        //Comment the lines 128-132 and uncomment 134-146, to input your own harvest values for the month
        var amt = Array( Array(11, 12, 34, 40, 40, 36, 17), 
                         Array(15, 12, 61, 8, 79, 10, 78), 
                         Array(12, 14, 13, 30, 2, 4, 36),
                         Array(87, 19, 23, 22, 31, 31, 40),
                        );

        // val amt = Array.ofDim[Int](row,col);
        // var input = new Scanner(System.in);

        //Inputing the values
        // for(i<-0 until row; j<-0 until col)
        // {
        //     print(i, j) 
        //     print(" Week "+ (i+1));
        //     print(" Day " + (j+1));
        //     print("  :Enter harvest ");
        //     amt(i)(j) = input.nextInt;   
            
        // }

        //Displaying the elements
        print("\n------------------------------------Statistics of Tea Harvest taken during a month (in kilograms)-------------------------------------------------\n");
        display(amt,col,row);
        print("\n-------------------------------------------------------------------------------------------------");
        //Calculating maximum value for each week
        getMax(amt,col,row);
        print("\n-------------------------------------------------------------------------------------------------");
        //Calculating minimum value for each week
        getMin(amt,col,row); 
        print("\n-------------------------------------------------------------------------------------------------");
        //Calculating average value for each week
        getAvg(amt,col,row)
        print("\n-------------------------------------------------------------------------------------------------");
        //Calculating median value for each week
        getMedian(amt,col,row);
        print("\n-------------------------------------------------------------------------------------------------");
    }
}
