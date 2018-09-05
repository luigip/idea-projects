package odds;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class KthElement {

    public static void main(String[] args) {
        ArrayList<Integer> myList = new ArrayList<Integer>(Arrays.asList(8, 2, 25, 96, 74, 5));
        System.out.println(findKthLargest(myList, 3));
    }
    
    static int findKthLargest(ArrayList<Integer> list, int k) {
        if (k == 1) 
            return largest(list);
        else {
            ArrayList<Integer> copy = (ArrayList<Integer>) list.clone();
            Integer largest = largest(copy);
            copy.remove(largest);
            return findKthLargest(copy, k - 1);
        } 
    }
    
    static int largest(List<Integer> list) {
        int max = Integer.MIN_VALUE;
        for (int i : list) {
            if (i > max)
                max = i;
        }
        return max;
    }
}
