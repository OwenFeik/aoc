import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

class Questions {
    private static int ALPHABET_LENGTH = 26;

    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(
            new FileReader("in.txt"))) {

            String line = null;
            int total = 0;
            // boolean[] questions = new boolean[ALPHABET_LENGTH];
            int[] questions = new int[ALPHABET_LENGTH];

            int count = -1;

            while ((line = br.readLine()) != null) {
                count++;

                System.out.println(line);
                if (line.equals("")) {
                    for (int i = 0; i < ALPHABET_LENGTH; i++) {
                        System.out.print((char) (i + (int) 'a'));
                        System.out.println(questions[i]);

                        if (questions[i] == count) {
                            total++;
                        }
                        questions[i] = 0;
                    }
                    System.out.println(count + "----");
                    count = -1;
                }
                else {
                    for (char c: line.toCharArray()) {
                        // questions[(int) c - (int) 'a'] = true;
                        questions[(int) c - (int) 'a'] += 1;
                    }
                }
            }

            System.out.println(total);
        }
        catch (IOException e) {}
    }
}
