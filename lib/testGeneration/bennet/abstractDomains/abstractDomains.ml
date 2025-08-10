module IT = IndexTerms
module LC = LogicalConstraints

module Make (GT : GenTerms.T) = struct
  let annotate (ctx : GenContext.Make(GT).t) : GenContext.Make(GT).t =
    (* Type-unsafe dispatch based on domain name *)
    (* This assumes GT.AD has the same representation as the matched interpreter's domain *)
    let module SafeInterpreter : Interpreter.Part with type AD.t = GT.AD.t = struct
      module AD = GT.AD

      (* UNSAFE: Determines module to use based on `name`. *)
      let interpreter : (module Interpreter.Part) =
        let module M =
          (val match AD.name with
               | "ownership" -> (module Ownership.Interpreter : Interpreter.Part)
               | _ -> failwith ("Unsupported domain: " ^ AD.name))
        in
        (* RUNTIME SAFETY CHECK *)
        assert (String.equal M.AD.name AD.name);
        (* ASSUME: GT.AD == M.AD *)
        (module M)


      (* UNSAFE: Uses [interpreter] *)
      let abs_assert (lc : LC.t) (d : AD.t) : AD.t =
        let module I = (val interpreter) in
        let unsafe_result : AD.t =
          let ownership_d = Obj.magic d in
          let result = I.abs_assert lc ownership_d in
          Obj.magic result
        in
        unsafe_result


      (* UNSAFE: Uses [interpreter] *)
      let abs_assign (assign_info : (IT.t * Sctypes.t) * IT.t) (d : AD.t) : AD.t =
        let module I = (val interpreter) in
        let unsafe_result : AD.t =
          (* ASSUME: GT.AD == M.AD *)
          let ownership_d = Obj.magic d in
          let result = I.abs_assign assign_info ownership_d in
          Obj.magic result
        in
        unsafe_result
    end
    in
    let module AI = Interpreter.Make (GT) (SafeInterpreter) in
    AI.annotate ctx
end

module Ownership : Domain.T = Ownership.Inner

(* module Interval = Interval.Inner  (* TODO: Fix module path *) *)
