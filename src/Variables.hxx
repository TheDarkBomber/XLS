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

#endif
