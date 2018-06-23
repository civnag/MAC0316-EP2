title "lol";
variables
    sample of float y;
    sample of string z;
    sample of (float,float) g;
    float p;
    int h;
    int j;
    string s;
    float t;
    boolean q;
    (int,int) tu;
end variables
commands
    print("lol");
    p := correlation({1.0,2.0,3.0},{2.0,4.0,6.0});
    h := 1;
    while h < 8 do
        h := (h+1);
        print("yyyyy " ++ toString(h));
        print(toString(h));
    end
    if 1 > 2 then
      print("if com else");
      if 2 > 1 then
        print("sem else");
      end
    else
      print("esse é o else");
    end
    g := {#(1.1,2.3),#(2.6,7.9)};
    print(toString(h));
    print(toString(q));
    print(g);
