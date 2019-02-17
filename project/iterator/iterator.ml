open Cfg

module Make(Domain : Domain.DOMAIN) = struct
  type env = Domain.t NodeMap.t
  type worklist = NodeSet.t

  let eval d = function
      CFG_skip _ -> d
    | CFG_assign (var, expr) -> Domain.assign d var expr
    | CFG_guard c | CFG_assert c -> Domain.guard d c
    | CFG_call _ -> d (* TODO: complete *)

  let rec iter env worklist = match NodeSet.elements worklist with
      [] -> env
    | node :: t ->
      let worklist' = NodeSet.of_list t in
      let env_d = try NodeMap.find node env with _ -> Domain.bottom in
      let env_d' = List.fold_left (fun acc arc ->
          let e = NodeMap.find arc.arc_src env in
          Domain.join acc (eval e arc.arc_inst)) env_d node.node_in in
      if Domain.subset env_d env_d' && Domain.subset env_d' env_d then
        iter env worklist'
      else
        let env' = NodeMap.add node env_d' env in
        let succs =
          List.map (fun a -> a.arc_dst) node.node_out |> NodeSet.of_list in
        iter env' (NodeSet.union succs worklist')

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
    iter env worklist
end
