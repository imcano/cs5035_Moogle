open Util ;;    
open CrawlerServices ;;
open Order ;;
open Pagerank ;;


(* RandomWalkRanker and QuantumRanker are for karma questions only *)
module MoogleRanker
  = InDegreeRanker (PageGraph) (PageScore)
  (*
     = RandomWalkRanker (PageGraph) (PageScore) (struct 
       let do_random_jumps = Some 0.20
       let num_steps = 1000
     end)
  *)
  (*  
   = QuantumRanker (PageGraph) (PageScore) (struct 
       let alpha = 0.01
       let num_steps = 1
       let debug = true
     end)
  *)

(* Dictionaries mapping words (strings) to sets of crawler links *)
module WordDict = Dict.Make(
  struct 
    type key = string
    type value = LinkSet.set
    let compare = string_compare
    let string_of_key = (fun s -> s)
    let string_of_value = LinkSet.string_of_set

    (* These functions are for testing purposes *)
    let gen_key () = ""
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = None
    let gen_value () = LinkSet.empty
    let gen_pair () = (gen_key(),gen_value())
  end)

(* A query module that uses LinkSet and WordDict *)
module Q = Query.Query(
  struct
    module S = LinkSet
    module D = WordDict
  end)

let print s = 
  let _ = Printf.printf "%s\n" s in
  flush_all();;


(***********************************************************************)
(*    PART 1: CRAWLER                                                  *)
(***********************************************************************)

(*Use crawlerservices.get_page to get a link from a page then deconstruct it and send in words for this function *)
let rec update_dict (words:string list) (link:link) (dict:WordDict.dict) : WordDict.dict =
  match words with
    | [] -> dict
    | word :: tl ->
      (*Look up the word in the dictionary *)
      match WordDict.lookup dict word with
        (*No match found in the dictionary add the link as the only value for that key*)
        | None -> update_dict tl link (WordDict.insert dict word (LinkSet.singleton link))
        (*Match is found in the dictionary insert that link into the set of links *)
        | Some s -> update_dict tl link (WordDict.insert dict word (LinkSet.insert link s))
;;

let rec update_frontier (links:link list) (frontier:LinkSet.set) : LinkSet.set =
  match links with
    | [] -> frontier
    | singleLink:: tl ->
      (*Look up the word in the dictionary *)
      match LinkSet.member frontier singleLink with
        (*No match found in the dictionary add the link as the only value for that key*)
        | false -> update_frontier tl (LinkSet.insert singleLink frontier)            
        (*Match is found in the dictionary insert that link into the set of links *)
        | true-> update_frontier tl frontier
;;



(* TODO: Build an index as follows:
 * 
 * Remove a link from the frontier (the set of links that have yet to
 * be visited), visit this link, add its outgoing links to the
 * frontier, and update the index so that all words on this page are
 * mapped to linksets containing this url.
 *
 * Keep crawling until we've
 * reached the maximum number of links (n) or the frontier is empty. *)
let rec crawl (n:int) (frontier: LinkSet.set)
    (visited : LinkSet.set) (d:WordDict.dict) : WordDict.dict = 
    (if n = 0 then d
    else 
      let x = LinkSet.choose frontier  in
      match x with
        |None-> d
        |Some (head,tail) -> if  LinkSet.member visited head  then
        crawl n tail  visited d else 
        let page = CrawlerServices.get_page head in
          match page with 
           |None ->   crawl n tail  visited d
           | Some link ->
              crawl 
              (n-1) 
              (update_frontier (link.links)  frontier)
              (LinkSet.insert head visited) 
              (update_dict (link.words) head d ))

;;

let crawler () = 
  crawl num_pages_to_search (LinkSet.singleton initial_link) LinkSet.empty
    WordDict.empty
;;

(* Debugging note: if you set debug=true in moogle.ml, it will print out your
 * index after crawling. *)
