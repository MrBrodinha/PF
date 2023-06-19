(*
Emanuel Pacheco, 47712
Fontes Externas Usadas:
- https://en.wikipedia.org/wiki/Netpbm
*)
(*Inputs-----------------------------------------------------------------------
p -> Tipo de imagem
tam -> Tamanho da imagem (como é quadrada, basta guardar um dos valores visto que é sempre tam*tam)   
*)
let p = read_line()
let tam = Scanf.sscanf (read_line()) " %d %d" (fun t1 t2 -> t1)

(*
Leio a linha, por exemplo "1 0 0 0"
Utilizo a função 'split_on_char' para separar a string em várias strings numa lista onde cada string tem um int, ficando neste exemplo ["1"; "0"; "0"; "0"]
Depois converto cada elemento da lista para inteiro e adiciono à matriz linha a linha
*)
let imagem = 
  let matrix = Array.make_matrix tam tam (-1) in
  for i = 0 to (tam-1) do 
    let temp = read_line() in
    let separar = String.split_on_char ' ' temp in

    for j = 0 to (tam-1) do 
      matrix.(i).(j) <- int_of_string (List.nth separar j);
    done;
  done;
  matrix
(*-----------------------------------------------------------------------------*)

(*W = 0 | B = 1*)
type color = W | B 
(* leaf of one color | node with four children *)
type image = L of color | N of image * image * image * image 

(*
Verifica se apenas uma cor existe ou não no quadrante enviado.
Basta comparar mos o primeiro elemento da matrix com os restantes, se algum for diferente
é porque o quadrante não é só de uma cor
*)
let rec uma_cor matrix x y =
  let n = Array.length matrix - 1 in

  if matrix.(0).(0) == matrix.(x).(y) then
    (
      if (y = n && x = n) then true
      else if (y != n) then uma_cor matrix x (y+1)
      else uma_cor matrix (x+1) 0
    )
  else false
  
(*
qN estão distribuido da seguinte forma
q1|q2
-----
q3|q4
É primeiro verificado se a matriz enviada (onde está guardado o nosso input) é só de uma cor, se sim, devolvemos W ou B dependente da cor, caso não,
dividimos esta matriz em 4 comoo ilustrado em cima, adicionamos os respetivos elementos da matriz principal para as 4 matrizes e chamamos
a função com um Nodo de 4 elementos, sendo cada um a chamada da função de forma recursiva com cada uma das matrizes, com o tamanho a metade
*)
let rec bti matrix =
  let n = Array.length matrix in 
  if (uma_cor matrix 0 0) then
    (
      if matrix.(0).(0) = 0 then L(W)
      else L(B)
    )
  else (
    let q1 = Array.make_matrix (n/2) (n/2) (-1) in
    let q2 = Array.make_matrix (n/2) (n/2) (-1) in
    let q3 = Array.make_matrix (n/2) (n/2) (-1) in
    let q4 = Array.make_matrix (n/2) (n/2) (-1) in

    for x = 0 to (n-1) do 
      for y = 0 to (n-1) do 
        if x < n/2 && y < n/2 then 
          (q1.(x).(y) <- matrix.(x).(y));

        if x < n/2 && y >= n/2 then 
          (q2.(x).(y - n/2) <- matrix.(x).(y));

        if x >= n/2 && y < n/2 then 
          (q3.(x - n/2).(y) <- matrix.(x).(y));

        if x >= n/2 && y >= n/2 then 
          (q4.(x - n/2).(y - n/2) <- matrix.(x).(y))
        done
      done;
      
      N (
        bti q1,
        bti q2,
        bti q3,
        bti q4
      )
    )

(*
Calcula o número de nodos ou folhas
Se quisermos o número de folhas, chamamos a função com n=0
Se quisermos o número de nodos, chamamos a função com n=1
*)
let rec numero quadtree n =
  match quadtree with
  | N (a, b, c, d) -> n + (numero a n) + (numero b n) + (numero c n) + (numero d n)
  | L (_) -> 1 - n

(*
Altura
Percorre a quadtree, se for um N chamamos 1 + os ramos todos de forma recursiva porque cada nodo tem obrigatoriamente 4 elementos
que acrescenta um de altura.
Podemos chamar a função com f=max e devolvemos a altura máxima, ou com f=min e devolvemos a altura minima
*)
let rec altura f quadtree =
  match quadtree with
  | N (a, b, c, d) -> 1 + f (f (f (altura f a) (altura f b)) (altura f c)) (altura f d)
  | L (_) -> 0

(*     
matrix temporária utilizada para dar print à quadtree
*)
let temp_matrix = Array.make_matrix tam tam (-1)

(*
print_matrix -> imprime a matriz
*)
let rec print_matrix matrix x y =
  let n = Array.length matrix - 1 in
  if (y = n && x = n) then
    (Printf.printf "%d\n" matrix.(x).(y))
  else if (y != n) then
    (Printf.printf "%d " matrix.(x).(y); print_matrix matrix x (y+1))
  else 
    (Printf.printf "%d\n" matrix.(x).(y); print_matrix matrix (x+1) 0)

(*
Guarda "Valor" (0 ou 1) nas posições do array entre x y e (x+n-1) (y+n-1)    
*)
let rec guardar_matrix matrix x y i j n valor = 
    if (i = n-1) && (j = n-1) then 
      (temp_matrix.(x+i).(y+j) <- valor)
    else if (j != n-1) then 
      (temp_matrix.(x+i).(y+j) <- valor; guardar_matrix matrix x y i (j+1) n valor;)
    else 
      (temp_matrix.(x+i).(y+j) <- valor; guardar_matrix matrix x y (i+1) 0 n valor)

(*
Função auxiliar para converter a quadtree para uma matriz
Percorremos a quadtree, e quando for um nodo, chamamos a função de forma recursiva para cada um dos ramos com as coordenadas minimas de cada quadrante, 
se for uma folha, adicionamos os respetivos valores à matriz entre as posições x y e (x+n-1) (y+n-1)
*)
let rec quadtree_to_matrix quadtree x y n =
    match quadtree with
    | N(a, b, c, d) -> 
    ( 
      quadtree_to_matrix a x y (n/2);
      quadtree_to_matrix b x (y+n/2) (n/2);
      quadtree_to_matrix c (x+n/2) y (n/2);
      quadtree_to_matrix d (x+n/2) (y+n/2) (n/2);
    )
    | L(W) -> guardar_matrix temp_matrix x y 0 0 n 0;
    | L(B) -> guardar_matrix temp_matrix x y 0 0 n 1

(*
Inverter W e B
Onde temos W metemos B e onde temos B metemos W
*)
let rec inverter quadtree =
  match quadtree with
  | L (W) -> L(B)
  | L (B) -> L(W)
  | N (a, b, c, d) -> N (inverter a, inverter b, inverter c, inverter d)

(*
Temos uma quadtree deste estilo:
q1|q2
-----
q3|q4
Vira a quadtree 90 para a esquerda se graus = 90, para a direita se for graus = 180 (Só funciona estes casos)
Se graus = 90 então fica
q2|q4
-----
q1|q3
Se graus=180 então fica
q4|q3 
-----
q2|q1
*)
let rec virar_quadtree quadtree graus =
  match quadtree, graus with
  | N (a, b, c, d), i when i = 180 -> N (virar_quadtree d 180, virar_quadtree c 180, virar_quadtree b 180, virar_quadtree a 180)
  | N (a, b, c, d), i when i = 90 -> N (virar_quadtree b 90, virar_quadtree d 90, virar_quadtree a 90, virar_quadtree c 90)
  | (L (c), _) -> L (c)
  | _ -> L (W) (*Caso inútil, nunca vai vir aqui*)

(*
"main"
Guarda a imagem no formato quadtree na variável qt
Imprime o número de folhas, o número de nodos, o tamanho mínimo e o tamanho máximo
Imprime a imagem virada 90º para a esquerda
Imprime a imagem invertida
Imprime a imagem virada 180º para a direita
*)
let () =
    let qt = ref (bti imagem) in
    Printf.printf "%d %d\n%d %d\n" (numero !qt 0) (numero !qt 1) (altura min !qt) (altura max !qt);

    Printf.printf "%s\n%d %d\n" p tam tam;
    qt := virar_quadtree !qt 90;
    quadtree_to_matrix !qt 0 0 tam;
    print_matrix temp_matrix 0 0;

    Printf.printf "%s\n%d %d\n" p tam tam;
    qt := inverter !qt;
    quadtree_to_matrix !qt 0 0 tam;
    print_matrix temp_matrix 0 0;

    Printf.printf "%s\n%d %d\n" p tam tam;
    qt := virar_quadtree !qt 180;
    quadtree_to_matrix !qt 0 0 tam;
    print_matrix temp_matrix 0 0;
  
(*
Exemplos de Inputs/Outputs

Input 1:
P1
8 8
1 1 1 1 1 1 1 1
1 1 0 0 0 0 0 0
1 1 0 0 0 0 0 0
1 1 1 1 1 1 1 1
1 1 1 1 1 1 1 1
1 1 0 0 0 0 0 0
1 1 0 0 0 0 0 0
1 1 1 1 1 1 1 1

52 17
2 3
P1
8 8
1 0 0 1 1 0 0 1 
1 0 0 1 1 0 0 1 
1 0 0 1 1 0 0 1 
1 0 0 1 1 0 0 1 
1 0 0 1 1 0 0 1 
1 0 0 1 1 0 0 1 
1 1 1 1 1 1 1 1 
1 1 1 1 1 1 1 1 
P1
8 8
0 1 1 0 0 1 1 0 
0 1 1 0 0 1 1 0 
0 1 1 0 0 1 1 0 
0 1 1 0 0 1 1 0 
0 1 1 0 0 1 1 0 
0 1 1 0 0 1 1 0 
0 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 
P1
8 8
0 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 
0 1 1 0 0 1 1 0 
0 1 1 0 0 1 1 0 
0 1 1 0 0 1 1 0 
0 1 1 0 0 1 1 0 
0 1 1 0 0 1 1 0 
0 1 1 0 0 1 1 0 

Input 2:
P1
8 8
1 0 1 0 1 0 1 0
0 1 0 1 0 1 0 1
1 0 1 0 1 0 1 0
0 1 0 1 0 1 0 1
1 0 1 0 1 0 1 0
0 1 0 1 0 1 0 1
1 0 1 0 1 0 1 0
0 1 0 1 0 1 0 1

Output 2:
64 21
3 3
P1
8 8
0 1 0 1 0 1 0 1 
1 0 1 0 1 0 1 0 
0 1 0 1 0 1 0 1 
1 0 1 0 1 0 1 0 
0 1 0 1 0 1 0 1 
1 0 1 0 1 0 1 0 
0 1 0 1 0 1 0 1 
1 0 1 0 1 0 1 0 
P1
8 8
1 0 1 0 1 0 1 0 
0 1 0 1 0 1 0 1 
1 0 1 0 1 0 1 0 
0 1 0 1 0 1 0 1 
1 0 1 0 1 0 1 0 
0 1 0 1 0 1 0 1 
1 0 1 0 1 0 1 0 
0 1 0 1 0 1 0 1 
P1
8 8
0 1 0 1 0 1 0 1 
1 0 1 0 1 0 1 0 
0 1 0 1 0 1 0 1 
1 0 1 0 1 0 1 0 
0 1 0 1 0 1 0 1 
1 0 1 0 1 0 1 0 
0 1 0 1 0 1 0 1 
1 0 1 0 1 0 1 0 
*)