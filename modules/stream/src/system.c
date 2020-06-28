#include "system.h"
#include "mod.h"

#include <module.h>

void
stream_system_init(struct vm *vm, struct stream_system *sys)
{
	paged_list_init(
			&sys->node_kinds,
			&vm->mem,
			sizeof(struct stream_node_kind));
}

struct stream_system *
stream_get_system(struct vm *vm)
{
	struct stream_mod_info *info;
	info = stream_mod_get_info(vm);

	return info->sys;
}

struct stream_node_kind *
stream_get_node_kind(struct stg_module *mod, struct atom *name)
{
	struct stream_system *sys;
	sys = stream_get_system(mod->vm);

	for (size_t i = 0; i < sys->node_kinds.length; i++) {
		struct stream_node_kind *kind;
		kind = paged_list_get(&sys->node_kinds, i);
		if (kind->mod_id == mod->id &&
				kind->name == name) {
			return kind;
		}
	}

	return NULL;
}

int
stream_register_node_kind(struct stg_module *mod, struct stream_node_kind kind)
{
	struct stream_system *sys;
	sys = stream_get_system(mod->vm);

	size_t id;
	id = paged_list_push(&sys->node_kinds);

	struct stream_node_kind *new_kind;
	new_kind = paged_list_get(&sys->node_kinds, id);
	*new_kind = kind;

	new_kind->mod_id = mod->id;

	return 0;
}
