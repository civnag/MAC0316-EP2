title "Testes do comando if";
variables
    sample of float porcentagens;
    float variancia;
    float minimo;
    int x;
    int y;
end variables
commands
    porcentagens := {12.4, 43.5, 53.3, 93.4};
    minimo := 50.0;
    variancia := variance(porcentagens);
    if(variancia < minimo) then
        print("A variância está muito alta");
    else
        print("A variância está de acordo");
    end

    x := toInt(mean(porcentagens));
    print(toString(x));
    y := 10;
    if((y + 17) != (x * 4)) then
        print("O resultado é diferente");
    else
        print("O resultado é igual");
    end

    if(((y + 17) != (x * 4)) && (y != x)) then
        print("O resultado é diferente");
    else
        print("O resultado é igual");
    end
