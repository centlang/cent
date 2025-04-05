#ifndef CENT_AST_SPAN_VALUE_H
#define CENT_AST_SPAN_VALUE_H

#include "cent/span.h"

namespace cent::ast {

template <typename ValueType> struct SpanValue {
    ValueType value;
    Span span;
};

} // namespace cent::ast

#endif
