open Cfg

module Make(Domain : Domain.DOMAIN) = struct
  type env = Domain.t NodeMap.t
  type worklist = NodeSet.t

  let path x y =
    let rec aux visited =
      function
        [] -> false
      | h :: t ->
        let succs = List.map (fun a -> a.arc_dst) h.node_out in
        if List.mem y succs then
          true
        else
          aux (visited @ succs) (t @ (List.filter (fun n -> not (List.mem n visited)) succs))
    in aux [x] [x]

  let widen_points cfg =
    let gotos = List.fold_left (fun acc n ->
        acc @ List.map (fun a -> a.arc_dst)
          (List.filter (fun a -> match a.arc_inst with
                 CFG_skip s when (String.length s >= 10 && String.equal (String.sub s 0 10) "skip: goto") -> true
               | _ -> false) n.node_out)) [] cfg.cfg_nodes in
    gotos
  (* List.fold_left (fun acc n ->
      let succs = List.map (fun a -> a.arc_dst) n.node_out in
      acc @ List.filter (fun n' -> path n' n) succs) [] cfg.cfg_nodes *)
  (* let main = List.find (fun f -> f.func_name = "main") cfg.cfg_funcs in
     let entry = main.func_entry in
     let rec aux visited result = function
      [] -> result
     | arc :: t ->
      let node = arc.arc_dst in
      let result', to_visit = 
        if NodeSet.mem node visited && path node arc.arc_src then
          NodeSet.add node result, t
        else
          result, (t @ node.node_out) in
      let visited' = NodeSet.add node visited in
      aux visited' result' to_visit in
     aux (NodeSet.singleton entry) NodeSet.empty entry.node_out *)

  let eval d = function
      CFG_skip _ -> d
    | CFG_assign (var, expr) -> Domain.assign d var expr
    | CFG_guard c | CFG_assert c -> Domain.guard d c
    | CFG_call _ -> d (* TODO: complete *)

  let rec iter env visited worklist = match NodeSet.elements worklist with
      [] -> env
    | node :: t ->
      let worklist' = NodeSet.of_list t in
      let env_d = try NodeMap.find node env with _ -> Domain.bottom in
      let op = if NodeSet.mem node visited then Domain.widen else Domain.join in
      let env_d' = List.fold_left (fun acc arc ->
          let e = NodeMap.find arc.arc_src env in
          op acc (eval e arc.arc_inst)) env_d node.node_in in
      let visited' = NodeSet.add node visited in
      if Domain.subset env_d env_d' && Domain.subset env_d' env_d then
        iter env visited' worklist'
      else
        let env' = NodeMap.add node env_d' env in
        let succs =
          List.map (fun a -> a.arc_dst) node.node_out |> NodeSet.of_list in
        iter env' visited' (NodeSet.union succs worklist')

  let init cfg =
    let init_entry = cfg.cfg_init_entry in
    let rec iter_2 env worklist = match NodeSet.elements worklist with
        [] -> env
      | node :: t ->
        let worklist' = NodeSet.of_list t in
        let env' = List.fold_left (fun acc arc ->
            Domain.join acc (eval acc arc.arc_inst)) env node.node_in in
        let succs =
          List.map (fun a -> a.arc_dst) node.node_out |> NodeSet.of_list in
        iter_2 env' (NodeSet.union succs worklist') in
    iter_2 (Domain.init cfg.cfg_vars) (NodeSet.singleton init_entry)

  let process_cfg cfg =
    let global_d = init cfg in
    (* let main = List.find (fun f -> f.func_name = "main") cfg.cfg_funcs in *)
    let worklist = NodeSet.of_list (cfg.cfg_nodes) in
    let env = NodeSet.fold (fun node acc ->
        NodeMap.add node global_d acc) worklist NodeMap.empty in
    iter env NodeSet.empty worklist
end
