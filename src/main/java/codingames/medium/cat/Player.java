package codingames.medium.cat;

import java.util.Scanner;

import static java.lang.Math.*;

class Player {

    private static final int R = 500;
    private static final double PIR = PI * R;

    enum CatDirection {
        NO, CLOCKWISE, COUNTERCLOCKWISE
    }

    static double euclidean(int ax, int ay, int bx, int by) {
        return sqrt(pow((double) bx - ax, 2) + pow((double) by - ay, 2));
    }

    private static final double VR = euclidean(0, 0, 500, 500);

    static double angle(int ax, int ay, int bx, int by, int cx, int cy) {
        int x1 = ax - bx;
        int x2 = cx - bx;
        int y1 = ay - by;
        int y2 = cy - by;
        double d1 = sqrt((double) x1 * x1 + y1 * y1);
        double d2 = sqrt((double) x2 * x2 + y2 * y2);
        return toDegrees(acos((x1 * x2 + y1 * y2) / (d1 * d2)));
    }

    static double px(double a) {
        return VR * cos(toRadians(a));
//        y = cy + r * sin(a)
    }

    static double py(double a) {
        return VR * sin(toRadians(a));
    }

    static double angleFi(int l) {      // угол по длине дуги
        return l * 180 / PIR;
    }

    static double arcLength(double fi) {    // длина дуги по углу
        return PIR * fi / 180;
    }

    static int incr(int target, int dep, int incr) {
        int incrAbs = dep > 0 ? -incr : dep < 0 ? incr : 0;
        return target + incrAbs;
    }

    static CatDirection catDirection(int initialCatX, int initialCatY, int catX, int catY) {
        if (initialCatX == catX && initialCatY == catY) {
            return CatDirection.NO;
        } else if (   (catX >= 0 && catY < initialCatY) || ((catX <= 0 && catY > initialCatY)) ||
                (catY == initialCatY && ((catY > 0 && catX > initialCatX) || (catY < 0 && catX < initialCatX))) ) {
            return CatDirection.CLOCKWISE;
        } else return CatDirection.COUNTERCLOCKWISE;
    }

    public static void main(String args[]) {
        Scanner in = new Scanner(System.in);
        in.nextInt();
        int targetX = 0;
        int targetY = 0;
        boolean go = false;
        double initialCatAngle = 0;
        double targetAngle = 180;
        int prevCatX = 0;
        int prevCatY = 0;

        // game loop
        while (true) {
            int mouseX = in.nextInt();
            int mouseY = in.nextInt();
            int catX = in.nextInt();
            int catY = in.nextInt();

            if (mouseX == 0 && mouseY == 0) {
                targetX = -catX;
                targetY = -catY;
                initialCatAngle = angle(500, 0, 0, 0, catX, catY);
                go = true;
            } else if (go) {
                CatDirection ctd = catDirection(prevCatX, prevCatY, catX, catY);
                if (ctd == CatDirection.COUNTERCLOCKWISE) {
                    targetAngle = initialCatAngle - 180 + 45;
                } else if (ctd == CatDirection.CLOCKWISE) {
                    targetAngle = initialCatAngle - 180 - 45;
                } else {
                    targetAngle = initialCatAngle - 180;
                }
                targetX = (int) px(targetAngle);
                targetY = (int) py(targetAngle);
            }
            System.out.println(targetX + " " + targetY + " " + targetX + " " + targetY + " " + catX + " " + catY);

            prevCatX = catX;
            prevCatY = catY;
        }
    }
}