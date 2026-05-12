(* ----------------------------------------------------------------
   Interface ocaml pour gbdd
   -------------------------------------------------------------------
   On garde les noms compatibles "cudd-idl" utilis�s dans lurette :

   Bdd._print ??

   Bdd.draw
   Bdd.nbminterms
   ---------------------------------------------------------------- *)

   (** BDD type. *)
type t

(* Init du module *)
external init_psz_verb : int -> bool -> unit = "gbdd_cml_init_with_psz_verb"

(** Inirialize the module. *)
let init : ?pagesize:int -> ?verbose:bool -> unit -> unit =
 fun ?(pagesize = 10000) ?(verbose = true) _ -> init_psz_verb pagesize verbose

(* Accès aux noeuds *)

(** Checks whenever the node is complemented. *)
external is_complemented : t -> bool = "gbdd_cml_is_complemented"

(** Returns root variable of the BDD. *)
external root_var : t -> int = "gbdd_cml_root_var"

(** Returns high (root variable if true) branch of the BDD. *)
external high_part : t -> t = "gbdd_cml_high_part"

(** Returns low (root variable is false) branch of the BDD. *)
external low_part : t -> t = "gbdd_cml_low_part"

(* Tests *)

(** Checks if BDD is a leaf (true of false). *)
external is_leaf : t -> bool = "gbdd_cml_is_leaf"

(** Checks if BDD is true constant. *)
  external is_true : t -> bool = "gbdd_cml_is_true"

(** Checks if BDD is false constant. *)
external is_false : t -> bool = "gbdd_cml_is_false"

(* Constantes *)

(** Returns BDD representing true. *)
external dtrue : unit -> t = "gbdd_cml_true"

(** Returns BDD representing false. *)
external dfalse : unit -> t = "gbdd_cml_false"

(** Returns BDD representing null (implementation detail). *) (* TODO: or at least I think so. *)
external null : unit -> t = "gbdd_cml_null"

(* Identité et Inverse *)
(** Returns BDD representing formula [var]. *)
external idy : int -> t = "gbdd_cml_idy"

(** Returns BDD representing formula [!var]. *)
external nidy : int -> t = "gbdd_cml_nidy"

(* Opérations booléennes *)

(** Inverts the BDD. *)
external dnot : t -> t = "gbdd_cml_not"

(** Returns BDD representing boolean OR operation applied. *)
external dor : t -> t -> t = "gbdd_cml_or"

(** Returns BDD representing boolean AND operation applied. *)
external dand : t -> t -> t = "gbdd_cml_and"

(** Returns BDD representing boolean XOR operation applied. *)
external xor : t -> t -> t = "gbdd_cml_xor"

(** Returns BDD representing boolean equality operation applied. *)
external eq : t -> t -> t = "gbdd_cml_eq"

(** Constructs BDD representing [if cond then x else y]. *)
external ite : t -> t -> t -> t = "gbdd_cml_ite"

(* Infos sur la structure *)
(** Number of nodes in a BDD. *)
external size : t -> int = "gbdd_cml_size"
external supportsize : t -> int = "gbdd_cml_supportsize"

(* quantification *)
external exist_local : t -> t -> t = "gbdd_cml_exist"
external forall_local : t -> t -> t = "gbdd_cml_forall"

let support_of_list vars =
  assert (vars <> []);
  List.fold_left
    (fun acc i -> dand acc (idy i))
    (idy (List.hd vars))
    (List.tl vars)

let (exist : int list -> t -> t) =
 fun vars bdd -> exist_local (support_of_list vars) bdd

let (forall : int list -> t -> t) =
 fun vars bdd -> forall_local (support_of_list vars) bdd

(* Extra *)
external print_mons : t -> unit = "gbdd_cml_print_mons"

(* compatibilité cudd *)

(** Returns root variable of the BDD. Same as [root_var]. *)
external topvar : t -> int = "gbdd_cml_root_var"


(** Returns then branch of the BDD. Same as [high_part]. *)
external dthen : t -> t = "gbdd_cml_high_part"

(** Returns else branch of the BDD. Same as [high_part]. *)
external delse : t -> t = "gbdd_cml_low_part"


(** Returns BDD representing formula [var]. Same as [idy]. *)
external ithvar : int -> t = "gbdd_cml_idy"

(** Checks if BDD is a constant (true of false). Same as [is_leaf]. *)
external is_cst : t -> bool = "gbdd_cml_is_leaf"

external support : t -> t = "gbdd_cml_cube"

(* Extra programmés directement en caml *)

let list_of_support (b : t) =
  let rec los x = if is_leaf x then [] else topvar x :: los (dthen x) in
  los (support b)
