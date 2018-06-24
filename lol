title "lol";
variables
    sample of float y;
    sample of string z;
    sample of (float,float) g;
    sample of int p;
    int h;
    int j;
    string s;
    float t;
    boolean q;
    (float,float) treta;
    (int,int) tu;
end variables
commands
    print("lol");
    p := {1,2,3};
    j := getInt(p, 1);
    print(toString(p));
    h := 1;
    while h < 8 do
        h := (h+1);
        print("yyyyy " ++ toString(h));
        print(toString(h));
    end
    treta := #(1.1,2.4);
    print(toString(treta));
    g := {#(1.1,2.3),#(2.6,7.9)};
    print(toString(h));
    print(toString(q));
    print(g);
