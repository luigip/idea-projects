package odds;

public class ScratchJava {
    
    private int legs = 4;
    
    static void legcounter(ScratchJava x) {
        System.out.println(x.legs);
    }

    public static void main(String[] args) {
        ScratchJava j = new ScratchJava();
        legcounter(j);
    }
}
