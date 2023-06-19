(*
Emanuel Pacheco, 47712

Fontes Externas Usadas:
      - https://cs.fit.edu/~ryan/cse1002/lectures/recursion.pdf
      - https://en.wikipedia.org/wiki/Motzkin_number
      - https://www.geeksforgeeks.org/motzkin-number/
      - https://dsheets.github.io/codoc/zarith.1.3/z/

Comando para compilar -> ocamlfind ocamlopt -linkpkg -package zarith ProblemaA.ml*)

let n = read_int() (*nº do utilizador*)

(*
Este array vai servir para guardar os valores da função "mn" enquanto os valores são calculados para evitar calculos repetidos,
sendo por exemplo o nº na posição "4" o valor de "mn 4"

Como esta função não pode ter valores negativos, inicializei a "-1" e asism basta verificar se a posição é "-1", 
caso não, é porque o valor já foi calculado
*)
let mem = Array.make (n+1) (Z.of_int (-1))

(*
Função que aplica a fórmula de Motzkin ao número n,
a fórmula usada foi econtrada na fonte 'https://www.geeksforgeeks.org/motzkin-number/'
e usa memoização para ir guardando os valores num array para a função ser mais otimizada  
*)
let rec mn n1 =
        if (mem.(Z.to_int n1) <> Z.of_int (-1)) then mem.(Z.to_int n1) 
        else (if n1 = Z.zero || n1 = Z.one then Z.one
              else let temp = Z.(((~$2 * n1 + ~$1) * mn (n1 - ~$1) + (~$3 * n1 - ~$3) * mn (n1 - ~$2)) / (n1 + ~$2)) in
              mem.(Z.to_int n1) <-  temp;
              temp)

let () = print_endline (Z.to_string (mn (Z.of_int n))) (*Printf do resultado*)

(*
Exemplo de Execução:
Se o utlizador insere por exemplo o nº 3, o que vai acontecer é o seguinte

É chamada a função 'mn n' com n = 3, e verifica primeiro se n = 3 existe no array, caso não vamos calcular o valor,
quando entra na função verifica se é igual a 0 ou 1 que no caso devolveria 1, mas como é 3 vai ao terceiro caso onde
vai aplicar a fórmula encontrada na fonte externa 'https://www.geeksforgeeks.org/motzkin-number/' e guardar esse valor numa
variável para depois mais tarde guardá la no array para no caso de este valor ser outra vez calculado não termos de calcula lo
outra vez.

A função é recursiva porque na fórmula estamos sempre a chamar a função com (n-1) e (n-2), ou seja no caso do 3 chamariamos
'mn 2' e 'mn 1', que dps com 'mn 1' devolveria 1 e com 'mn 2' iria outra vez ao terceiro caso calcular com a fórmula e chamaria
'mn 1' e 'mn 0', que seria respetivamente 1 e 1.
*)
