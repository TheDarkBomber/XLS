#include "Variables.hxx"
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
