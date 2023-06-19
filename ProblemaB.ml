(* Emanuel Pacheco, 47712
Fontes externas usadas:
- https://en.wikipedia.org/wiki/Knapsack_problem
- https://www.geeksforgeeks.org/0-1-knapsack-problem-dp-10/
*)

(*Inputs-----------------------------------------------------
num -> Tamanho do bolo
m -> Número de fatias
preco -> Array de tamanho num onde vai ser guardado os preços, onde o indice representa o tamanho (começa em 0 até m-1)
         (inicializado a 0 porque se algum tamanho leva skip fica a 0)
*)
let num = read_int()
let m = read_int()
let preco = Array.make num 0

let () =
  for k = 1 to m do
      let i, j =
        Scanf.sscanf (read_line()) " %d %d" (fun i j -> (i, j))
      in
      preco.(i-1) <- j;
    done
(*-----------------------------------------------------------*)

(*
Devolve o maior entre x e y   
*)
let max x y = if x > y then x else y


(*
profito arr n
arr -> array com os preços
n -> tamanho do bolo

É criado um array temporário onde vai ser guardado os preços máximos possíveis em cada posição, em que o indice de cada posição 
corresponde ao tamanho da fatia.

Ou seja ele vai calcular o maior preço possível para cada possição, digamos no caso que o tamanho 1 tem preço 4 e o tamanho 2 tem preço 5,
como 2 é 1+1, 2 fica com o valor 8 visto que este é o maior preço possível para o tamanho 2
*)
let profito arr n =
  let arr_temp = Array.make (n+1) 0 in

  for i = 1 to n do 
    let max_val = ref min_int in

    for j = 0 to (i-1) do
        max_val := (max !max_val (arr.(j) + arr_temp.(i-j-1)));
    done;

    arr_temp.(i) <- !max_val;
  done;
  arr_temp.(n)

(*
Onde é efetuada a chamada da função e é devolvido o output para o terminal   
*)
let () =
  print_int (profito preco num);
  print_newline()

(*Exemplo inputs/outputs
Input 1:
8
8
1 1
2 2
3 3
4 4
5 5
6 6
7 7
8 8
Output 1:
8

Input 2:
8
5
1 5
2 5
3 5
7 5
8 5
Output 2:
40
*)





