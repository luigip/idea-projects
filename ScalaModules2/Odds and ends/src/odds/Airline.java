package odds;

import java.util.Scanner;

public class Airline {

    private static final int first0 = 1,  first1 = 5, economy0 = 6,  economy1 = 10;
    private boolean[] seating = new boolean[economy1 + 1];
    private Scanner input = new Scanner(System.in);
       
    void run() {
        while (true) {
            System.out.println("Please type 1 for First Class or 2 for Economy: ");
            int section = input.nextInt();
            bookSeat(section, true);
        }
    }
    
    void bookSeat(int section, boolean firstTry) {
        switch (section) {
            case 2: assignSeat(economy0, economy1, "Economy", "First Class", 1, firstTry); break;
            case 1: assignSeat(first0,   first1,   "First Class", "Economy", 2, firstTry); break;                         
        }
    }
    
    void assignSeat(int lower, int upper, String thisSection, String otherSection, int other, boolean firstTry) {
        int seatNumber = findSeat(lower, upper);
        if (seatNumber == -1) {
            System.out.println(thisSection + " is fully booked.");
            if (firstTry) {
                System.out.println("Would you like " + otherSection + "? 1 for Yes 2 for No");
                if (input.nextInt() == 1) 
                    bookSeat(other, false);
                else System.out.println("Next flight leaves in 3 hours.");
            }
            else System.out.println("Both sections full!");
        }
        else {
            seating[seatNumber] = true;
            System.out.println("Seat booked: " + seatNumber);
        }
    }
    
    int findSeat(int lower, int upper) {
        int seatNumber = -1;
        for (int i = lower; seatNumber == -1 && i <= upper; i++) {
            if (seating[i] == false)
                seatNumber = i;
        }
        return seatNumber;
    }

    public static void main(String[] args) {
        new Airline().run();
    }

}
