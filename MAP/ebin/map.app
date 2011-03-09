{application, map,
	[{description, "Mobile Application Part"},
		{vsn, "0.1"},
		{modules, [map, map_app, map_sup, map_pm_sup,
				map_load_fsm, map_dsm_fsm, map_psm_fsm, map_rsm_fsm]},
		{registered, []},
		{applications, [kernel, stdlib, tcap]},
		{env, [{supref, {local, map_sup}}]},
		{mod, {map_app, []}}]}.
