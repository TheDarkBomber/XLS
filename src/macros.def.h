#ifndef __MACROS_XLS_DEF_
#define __MACROS_XLS_DEF_

#define JMPIF(lhs, rhs, label) if ((lhs) == (rhs)) goto label;
#define UQP(type) std::unique_ptr<type>
#define MUQ(type, ...) std::make_unique<type>(__VA_ARGS__);
#define MDU(type) std::_MakeUniq<type>::__single_object
#define VDX(atype, btype) std::vector<std::pair<atype, btype>>
#define CMP(a, b) !std::string(a).compare(b)
// std::_MakeUniq<DwordExpression>::__single_object result = std ::make_unique<DwordExpression>(CurrentInteger);
#endif
