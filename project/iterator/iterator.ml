open Cfg

module Make(Domain : Domain.DOMAIN) = struct
  let eval d = function
      CFG_skip _ -> d
    | CFG_assign (var, expr) -> Domain.assign d var expr
    | CFG_guard c -> Domain.guard d c
    | CFG_assert c -> d (* TODO: complete *)
    | CFG_call f -> d (* TODO: complete *)

  let init cfg =
    let init_entry = cfg.cfg_init_entry in
    let global_domain = List.fold_left (fun acc x -> eval acc x.arc_inst)
        (Domain.init cfg.cfg_vars) init_entry.node_out in
    global_domain
end
