#include "Type.hxx"
#include "Parser.hxx"

dword GetCountof(XLSType type) {
	if (type.Size == 0) return 0;
	if (type.IsStruct) {
		dword total = 0;
		for (auto value : type.Structure.Fields) {
			total += GetCountof(value.second.second);
		}
		return total;
	}
	return 1;
}
