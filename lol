title "lol";
variables
    (int,int,int) tu;
    sample of float y;
    sample of string z;
    float p;
    int h;
    int j;
    string s;
    float t;
    boolean q;
end variables
commands
    print("lol");
    p := correlation({1.0,2.0,3.0},{2.0,4.0,6.0});
    h := 1;
    j := toInt(3.0);
    t := pow(2.0,2.0);
    tu := #(1,2,3);
    print("Teste: " ++ toString(j));
    while h < 8 do
        h := (h+1);
        print("yyyyy " ++ toString(h));
        print(toString(h));
    end
    print(toString(h));
    print(tu);
