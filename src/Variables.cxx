#include "Variables.hxx"
#include "Parser.hxx"
#include "Type.hxx"

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
	llvm::StoreInst* storeInstance = Builder->CreateStore(Cast(variable.Type, value), variable.Value);
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
	SSA* V = Builder->CreateLoad(variable.Type.Type, variable.Value, variable.Name.c_str());
	if (variable.Type.IsRangedPointer) {
		variable.Name += "(extractedptr)";
		variable.Type = DefinedTypes[variable.Type.Dereference + "*"];
		// TODO: boundary check ranged pointers
		V = Builder->CreateExtractValue(V, llvm::ArrayRef<unsigned>(RANGED_POINTER_VALUE));
	}

	if (variable.Type.IsVector) {
		SSA* R = Builder->CreateExtractElement(V, index);
		R = Cast(DefinedTypes[variable.Type.Dereference], R);
		TypeAnnotation[R] = DefinedTypes[variable.Type.Dereference];
		return R;
	}

	SSA* GEP = Builder->CreateInBoundsGEP(DefinedTypes[variable.Type.Dereference].Type, V, index);
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
	SSA* castedValue = Cast(DefinedTypes[variable.Type.Dereference], value);
	SSA* V = Builder->CreateLoad(variable.Type.Type, variable.Value, variable.Name);
	if (variable.Type.IsRangedPointer) {
		variable.Name += "(extractedptr)";
		variable.Type = DefinedTypes[variable.Type.Dereference + "*"];
		// TODO: boundary check ranged pointers
		V = Builder->CreateExtractValue(V, llvm::ArrayRef<unsigned>(RANGED_POINTER_VALUE));
	}

	if (variable.Type.IsVector) {
		SSA* R = Builder->CreateInsertElement(V, castedValue, index);
		TypeAnnotation[R] = variable.Type;
		WriteVariable(R, variable, volatility);
		return R;
	}

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
	SSA* castedValue = Cast(fieldType, value);
	XLSVariable exdexedVariable {.Type = fieldType, .Value = GEP, .Name = ".wfield@#" + variable.Name};
	(void)WriteVariable(castedValue, exdexedVariable, volatility);
	TypeAnnotation[castedValue] = fieldType;
	return castedValue;
}

SSA* ExdexRangedPointerCount(SSA* value, XLSVariable variable) {
	std::vector<SSA*> GEPIndex(2);
	GEPIndex[0] = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, 0, false));
	GEPIndex[1] = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, RANGED_POINTER_COUNTOF, false));
	SSA* GEP = Builder->CreateGEP(variable.Type.Type, variable.Value, GEPIndex);
	XLSVariable exdexedVariable {.Type = DefinedTypes["#addrsize"], .Value = GEP, .Name = ".setcountof@#" + variable.Name};
	(void)WriteVariable(value, exdexedVariable);
	return value;
}

XLSVariable DemoteVariable(XLSVariable variable) {
	while (variable.Type.IsPointer || variable.Type.IsRangedPointer) {
		SSA* V = Builder->CreateLoad(variable.Type.Type, variable.Value, variable.Name.c_str());
		if (variable.Type.IsRangedPointer) {
			variable.Name += "(extractedptr)";
			variable.Type = DefinedTypes[variable.Type.Dereference + "*"];
			V = Builder->CreateExtractValue(V, llvm::ArrayRef<unsigned>(RANGED_POINTER_VALUE));
		}
		SSA* GEP = Builder->CreateInBoundsGEP(DefinedTypes[variable.Type.Dereference].Type, V, ZeroSSA(DefinedTypes["dword"]));
		variable.Type = DefinedTypes[variable.Type.Dereference];
		variable.Value = GEP;
		TypeAnnotation[GEP] = variable.Type;
	}

	variable.Name += "(demoted)";
	return variable;
}

SSA* DemotePointer(XLSType type, SSA* expression) {
	SSA* demoted = expression;
	while (type.IsPointer || type.IsRangedPointer) {
		if (type.IsRangedPointer) {
			type = DefinedTypes[type.Dereference + "*"];
			demoted = Builder->CreateExtractValue(demoted, llvm::ArrayRef<unsigned>(RANGED_POINTER_VALUE));
		}
		type = DefinedTypes[type.Dereference];
		SSA* GEP = Builder->CreateInBoundsGEP(type.Type, demoted, ZeroSSA(DefinedTypes["dword"]));
		demoted = Builder->CreateLoad(type.Type, GEP, "(demoted pointer)");
	}

	TypeAnnotation[demoted] = type;
	return demoted;
}

XLSVariable FetchVirtualVariable(VariableExpression* variable) {
	XLSVariable output;
	output = AllonymousValues[variable->GetName()];
	if (variable->GetOffset() == nullptr && variable->GetField() == "") return output;
	if (variable->GetOffset() != nullptr) {
		SSA* index = variable->GetOffset()->Render();
		output.Name += "(offset)";
		SSA* GEP = Builder->CreateGEP(output.Type.Type, output.Value, index);
		output.Value = GEP;
	}
	if (variable->GetField() != "") {
		std::vector<SSA*> GEPIndex(2);
		auto fieldData = output.Type.Structure.Fields[variable->GetField()];
		if (output.Type.IsPointer || output.Type.IsRangedPointer) output = DemoteVariable(output);
		output.Name += "(field)";
		GEPIndex[0] = ZeroSSA(DefinedTypes["dword"]);
		GEPIndex[1] = llvm::ConstantInt::get(*GlobalContext, llvm::APInt(32, fieldData.first, false));
		SSA* GEP = Builder->CreateGEP(output.Type.Type, output.Value, GEPIndex);
		output.Type = fieldData.second;
		output.Value = GEP;
	}
	return output;
}
