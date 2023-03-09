#include "Variables.hxx"
#include "Parser.hxx"
#include "num.def.h"
#include <llvm/IR/Instructions.h>

std::map<std::string, XLSVariable> AllonymousValues;

SSA* ReadVariable(XLSVariable variable, bool volatility) {
	llvm::LoadInst* loadInstance = Builder->CreateLoad(variable.Type.Type, variable.Value, variable.Name.c_str());
  loadInstance->setVolatile(volatility);
  TypeAnnotation[loadInstance] = variable.Type;
	return loadInstance;
}

SSA* ReadVariable(std::string name, bool volatility) {
	if (AllonymousValues.find(name) == AllonymousValues.end())
		return CodeError("Attempt to read undeclared variable.");
	return ReadVariable(AllonymousValues[name], volatility);
}

SSA* WriteVariable(SSA* value, XLSVariable variable, bool volatility) {
  llvm::StoreInst* storeInstance = Builder->CreateStore(ImplicitCast(variable.Type, value), variable.Value);
  storeInstance->setVolatile(volatility);
  TypeAnnotation[storeInstance] = variable.Type;
  return value;
}

SSA* WriteVariable(SSA* value, std::string name, bool volatility) {
	if (AllonymousValues.find(name) == AllonymousValues.end())
		return CodeError("Attempt to write undeclared variable.");
	return WriteVariable(value, AllonymousValues[name], volatility);
}

SSA* AddrVariable(XLSVariable variable) {
  XLSType PtrType;
  PtrType.Dereference = variable.Type.Name;
  PtrType.Name = variable.Type.Name;
  PtrType.Name.push_back('*');
  PtrType.IsPointer = true;
  PtrType.Type = variable.Type.Type->getPointerTo();
  PtrType.Size = GlobalLayout->getPointerSizeInBits();
  if (!CheckTypeDefined(PtrType.Name))
    return nullptr;
  TypeAnnotation[variable.Value] = PtrType;
  return variable.Value;
}

SSA* AddrVariable(std::string name) {
  if (AllonymousValues.find(name) == AllonymousValues.end())
    return CodeError("Unknown variable name to address.");
	return AddrVariable(AllonymousValues[name]);
}

SSA* IndexVariable(XLSVariable variable, SSA* index, bool volatility) {
  SSA *V = Builder->CreateLoad(variable.Type.Type, variable.Value, variable.Name.c_str());
  SSA *GEP = Builder->CreateInBoundsGEP(DefinedTypes[variable.Type.Dereference].Type, V, index);
	XLSVariable indexedVariable {.Type = DefinedTypes[variable.Type.Dereference], .Value = GEP, .Name = ".index@#" + variable.Name};
  return ReadVariable(indexedVariable, volatility);
}

SSA* IndexVariableField(XLSVariable variable, XLSType fieldType, SSA* index, bool volatility) {
  std::vector<SSA*> GEPIndex(2);
  GEPIndex[0] = ZeroSSA(DefinedTypes["dword"]);
  GEPIndex[1] = index;
  SSA* GEP = Builder->CreateGEP(variable.Type.Type, variable.Value, GEPIndex);
	XLSVariable indexedVariable {.Type = fieldType, .Value = GEP, .Name = ".rfield@#" + variable.Name};
	return ReadVariable(indexedVariable, volatility);
}

SSA* IndexField(XLSType type, std::string field, SSA* expression) {
	if (!type.IsStruct) return CodeError("Attempt to index field of non-structure type.");
	StructData structure = type.Structure;
	if (structure.Fields.find(field) == structure.Fields.end())
		return CodeError("Field not found.");

	SDX(dword, XLSType) XLSField = structure.Fields[field];
	SSA* extractedValue = Builder->CreateExtractValue(expression, llvm::ArrayRef<unsigned>(XLSField.first));
	TypeAnnotation[extractedValue] = XLSField.second;
	return extractedValue;
}

SSA* ExdexVariable(SSA* value, XLSVariable variable, SSA* index, bool volatility) {
  SSA* castedValue = ImplicitCast(DefinedTypes[variable.Type.Dereference], value);
  SSA* V = Builder->CreateLoad(variable.Type.Type, variable.Value, variable.Name);
  SSA* GEP = Builder->CreateInBoundsGEP(DefinedTypes[variable.Type.Dereference].Type, V, index);
	XLSVariable exdexedVariable {.Type = DefinedTypes[variable.Type.Dereference], .Value = GEP, .Name = ".exdex@#" + variable.Name};
	(void)WriteVariable(castedValue, exdexedVariable, volatility);
	TypeAnnotation[castedValue] = exdexedVariable.Type;
  return castedValue;
}

SSA* ExdexVariableField(SSA* value, XLSVariable variable, XLSType fieldType, SSA* index, bool volatility) {
  std::vector<SSA *> GEPIndex(2);
  GEPIndex[0] = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, 0, false));
  GEPIndex[1] = index;
  SSA* GEP = Builder->CreateGEP(variable.Type.Type, variable.Value, GEPIndex);
  SSA* castedValue = ImplicitCast(fieldType, value);
	XLSVariable exdexedVariable {.Type = fieldType, .Value = GEP, .Name = ".wfield@#" + variable.Name};
	(void)WriteVariable(castedValue, exdexedVariable, volatility);
  TypeAnnotation[castedValue] = fieldType;
  return castedValue;
}
