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
      let env_d = try NodeMap.find node env with Not_found -> Domain.bottom in
      let env_d' = List.fold_left (fun acc arc ->
          Domain.join acc (eval acc arc.arc_inst)) env_d node.node_in in
      if Domain.subset env_d env_d' && Domain.subset env_d' env_d then
        iter env worklist'
      else
        let env' = NodeMap.add node env_d' env in
        iter env' worklist'

  let init cfg =
    let init_entry = cfg.cfg_init_entry in
    List.fold_left (fun acc x -> eval acc x.arc_inst)
      (Domain.init cfg.cfg_vars) init_entry.node_out

  let process_cfg cfg =
    let global_d = init cfg in
    let worklist = NodeSet.of_list cfg.cfg_nodes in
    let env = NodeSet.fold (fun node acc ->
        NodeMap.add node global_d acc) worklist NodeMap.empty in
    iter env worklist
end
