package odds;

import java.util.ArrayList;
import java.util.List;

public class Divider {
   
    public static int[] divide(int n) {
        return recurse(n, new ArrayList<Integer>());
    }
    
    private static int[] recurse(int n, List<Integer> list) {
        if (n == 0) 
            return listToIntArray(list);
        else {
          list.add(0, n % 10);
          return recurse(n / 10, list);
        }   
    }
            
    private static int[] listToIntArray(List<Integer> list) {
        int[] a = new int[list.size()];
        for(int i = 0; i < list.size(); i++) {
            a[i] = list.get(i);
        }
        return a;
    }
    
    // Test
    public static void main(String[] args) {
        int n = 12345;
        int[] r = divide(n);
        for (int i : r)
            System.out.print(i + " ");
    }    
}
