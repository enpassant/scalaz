import java.util.List;

public class Number {
    public static void printPersons(List<Person> roster, CheckPerson tester) {
        for (Person p : roster) {
            if (tester.test(p)) {
                p.printPerson();
            }
        }
    }

    public static void main(List<Person> roster) {
        printPersons(roster,
            (Person p) -> p.getGender() == Person.Sex.MALE
            && p.getAge() >= 18
            && p.getAge() <= 25);
    }

    interface CheckPerson {
        boolean test(Person p);
    }

    static class Person {
        public enum Sex {
            MALE, FEMALE
        }

        String name;
        Sex gender;
        String emailAddress;

        public Sex getGender() {
            return gender;
        }

        public int getAge() {
            return 10;
        }

        public Person(String name) {
            this.name = name;
        }

        public void printPerson() {
            System.out.println("Name: " + name);
        }
    }
}
