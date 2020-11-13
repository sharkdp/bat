import java.util.Scanner;

/* This Java program was submiited to help bat
 * with its syntax highlighting tests
 */

public class Main
{
    public static void main(String[] arg)
    {
        Scanner st = new Scanner(System.in);
        int t;
        t = st.nextInt();
        String tem;
        tem = st.nextLine();
        for(int zz=0;zz<t;zz++)
        {
            String str;
            str = st.nextLine();
            int n = str.length();
            char ch;
            for(int i=0;i<n;i++)
            {
                ch = str.charAt(i);
                if(ch=='a')
                System.out.print("n");
                else if(ch=='s')
                System.out.print("i");
                else
                System.out.print(ch);
            }
            System.out.println();
        }
        while(t!=0) {
            // Decrement t
            t -= 1;
        }
    }
}
