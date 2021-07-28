//Harvest Statistics taken during a month. Program to get the maximum, minimum, average and median of month displaying the week with the day

import java.util.Scanner;

object Harvest {
    //function to display elements
    def display(amt: Array[Array[Int]],col:Int,row:Int) : Unit =
        {
            print("\n\t");

            for (j<-0 until col){
                j match{
                                case 0 => print(" |  Mon ");
                                case 1 => print(" |  Tue  ");
                                case 2 => print(" |  Wed ");
                                case 3 => print(" |  Thu ");
                                case 4 => print(" |  Fri ");
                                case 5 => print(" |  Sat ");
                                case _  => print(" |  Sun ");
                            }
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
    def getPos(amt: Array[Array[Int]],col:Int,row:Int,value:Int): Unit =
        {
            for(i<-0 until row){
                
                    for(j<-0 until col)
                    {
                        if(amt(i)(j) == value)
                        {
                            print(" : WEEK " + (i+1));
                            var day = ((i * 7) + j + 1);
                            print(" => ");
                            print(" " + day);
                            if(day%10 == 1)
                                print(" st");
                            else if(day%10 == 2)
                                print(" nd");
                            else if(day%10 == 3)
                                print(" rd"); 
                            else 
                                print(" th");   
                            j match{
                                case 0 => print(" Monday");
                                case 1 => print(" Tueday");
                                case 2 => print(" Wednesday");
                                case 3 => print(" Thursday");
                                case 4 => print(" Friday");
                                case 5 => print(" Saturday");
                                case _  => println(" Sunday");
                            }
                            
                        }
                           
                    }
                
            }
        }
    
    //function to find average of each week
    def getAvg(amt: Array[Array[Int]],col:Int,row:Int) : Unit =
        {
            var avg: Float = 0.00f;
            var days: Float = (col*row).toFloat;
            var sum = 0;
            for(i<-0 until row; j<-0 until col)
            {
                sum += amt(i)(j);       
            }
            avg = (sum / days);
            var avg_short = (avg * 1000).toInt / 1000F;
            print("\nAverage = " + avg_short + " kg per day");
        }
    //function to find median of each week
    def getMedian(amt: Array[Array[Int]],col:Int,row:Int) : Unit =
    {  
        var median: Float = 0.00f;
        val sorted_amt = Array.ofDim[Int](row*col);
        for(i<-0 until row; j<-0 until col)
        {
            sorted_amt((i*7) + j) = amt(i)(j);
        }
        
            for(j <- 0 until row*col-1){
                // loop for comparison and swapping
                for(k <- 0 until row*col-j-1){
                    if(sorted_amt(k+1) < sorted_amt(k))
                    {
                        // swapping of elements
                        var temp = sorted_amt(k);
                        sorted_amt(k) = sorted_amt(k+1);
                        sorted_amt(k+1) = temp;
                    }
                } 
            }      
        var mid = ((col*row)/2); //14th element Index = 13
        var divider:Float = 2.00f;
        median = ((sorted_amt(mid-1)+sorted_amt(mid))/divider); // median of 14th and 15th elements of the sorted array
        print("\nMedian = " + median);
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
        //Calculating maximum value            
        
                var max = 0; 
                for(i<-0 until row; j<-0 until col)
                {
                    if(j == 0)
                        max = 0;               
                    if(amt(i)(j) > max)
                        max = amt(i)(j);                            
                }
                print("\nMaximum = " + max);
                print(" kg");
                getPos(amt,col,row,max);
            
        print("\n-------------------------------------------------------------------------------------------------");
        //Calculating minimum value 
        
                var min = 100;
                for(i<-0 until row; j<-0 until col)
                {                 
                    if(amt(i)(j) < min)
                        min = amt(i)(j);            
                }
                print("\nMinimum = " + min);
                print(" kg"); 
                getPos(amt,col,row,min);
              
        print("\n-------------------------------------------------------------------------------------------------");
        //Calculating Range
        var range = max - min;
        print("\nRange = " + range);
        print("\n-------------------------------------------------------------------------------------------------");
        //Calculating average value 
        getAvg(amt,col,row)
        print("\n-------------------------------------------------------------------------------------------------");
        //Calculating median value 
        getMedian(amt,col,row);
        print("\n-------------------------------------------------------------------------------------------------");
    }
}
