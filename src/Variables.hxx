#ifndef __VARIABLES_XLS_H_
#define __VARIABLES_XLS_H_
#include "Parser.hxx"
#include <map>

struct XLSVariable {
	XLSType Type;
	SSA* Value;
	bool Global = false;
	std::string Name;
};

extern std::map<std::string, XLSVariable> AllonymousValues;

SSA* ReadVariable(XLSVariable variable, bool volatility = false);
SSA* ReadVariable(std::string name, bool volatility = false);

SSA* AddrVariable(XLSVariable variable);
SSA* AddrVariable(std::string name);

SSA* WriteVariable(SSA* value, XLSVariable variable, bool volatility = false);
SSA* WriteVariable(SSA* value, std::string name, bool volatility = false);

SSA* IndexVariable(XLSVariable variable, SSA* index, bool volatility = false);
SSA* IndexVariableField(XLSVariable variable, XLSType fieldType, SSA* index, bool volatility = false);

SSA* IndexField(XLSType type, std::string field, SSA* expression);

SSA* ExdexVariable(SSA* value, XLSVariable variable, SSA* index, bool volatility = false);
SSA* ExdexVariableField(SSA* value, XLSVariable variable, XLSType fieldType, SSA* index, bool volatility = false);

SSA* ExdexRangedPointerCount(SSA *value, XLSVariable variable);

XLSVariable DemoteVariable(XLSVariable variable);
SSA* DemotePointer(XLSType type, SSA* expression);

#endif
