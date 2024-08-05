#include "ast.hpp"

void LocalDef::sem() {
    if (fun == nullptr) {
        var->sem();
    } else {
        fun->sem();
    }
}