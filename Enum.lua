--[[
	This file is a part of the SLVM module.

	SLVM - SecureLuaVirtualMachine
	Copyright (C) 2021 ASense

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program. If not, see <http://www.gnu.org/licenses/>.
	
	SLVM | By the ASense Team
]]

local Types = require(script.Parent:WaitForChild("Types"))
local EnumList: Types.SLVMEnum = shared.SLVMEnums or {}

--// Lazy Functions
local function RecursiveFreeze(T: {[any]: any?}, Forbidden: {string})
	if not table.isfrozen(T) then
		table.freeze(T)
	end
	
	for Index, Value in T do
		if type(Value) == "table" and (not table.find(Forbidden, Index)) then
			RecursiveFreeze(Value, Forbidden)
		end
		
		task.wait()
	end
end

local function GetDictionaryLength(T: {[any]: any?})
	local Count = 0
	
	for _, _ in pairs(T) do
		Count += 1
	end
	
	return Count
end

local function GetFirstElementOfTable(T: {[any]: any?})
	for _, Value in pairs(T) do
		return Value
	end
end

local function AddSubEnum(Name: string): Types.SLVMSubEnum
	EnumList[Name] = {}
	
	return EnumList[Name]
end

local function AddEnumItem(SubEnum: Types.SLVMSubEnum, Name: string): Types.SLVMEnumItem
	SubEnum[Name] = {
		Name = Name,
		Value = GetDictionaryLength(SubEnum) + 1,
		EnumType = SubEnum,
	}

	return SubEnum[Name]
end

--// Utilities Library
local Utilities = {}

function Utilities:IsA(Object: any, EnumClass: string)
	if type(Object) == "table" then
		if Object.Name and EnumClass == "EnumItem" then
			return true
		end
		
		if EnumClass == "Enum" then
			return true
		end
		
		local FirstElement = GetFirstElementOfTable(Object)
		if
			type(FirstElement) == "table" and FirstElement.Name
			and EnumClass == "SubEnum"
		then
			return true
		end
	end
	
	return false
end

function Utilities:FindEnumItemByValue(SubEnum: Types.SLVMSubEnum, Value: number): Types.SLVMEnumItem?
	for _, EnumItem in pairs(SubEnum) do
		if EnumItem.Value == Value then
			return EnumItem
		end
	end
end

function Utilities:FindEnumItemByName(SubEnum: Types.SLVMSubEnum, Name: string): Types.SLVMEnumItem?
	for _, EnumItem in pairs(SubEnum) do
		if EnumItem.Name == Name then
			return EnumItem
		end
	end
end

if not shared.SLVMEnums then
	--// Enum Structure Creation
	--> Rule
	local RuleType = AddSubEnum("RuleType")
	AddEnumItem(RuleType, "ClosureRestriction")
	AddEnumItem(RuleType, "UserDataRestriction")
	AddEnumItem(RuleType, "TableRestriction")
	AddEnumItem(RuleType, "PropertyRestriction")
	AddEnumItem(RuleType, "TypeRestriction")

	--> Variable Types
	local VariableTypes = AddSubEnum("VariableType")
	AddEnumItem(VariableTypes, "Number")
	AddEnumItem(VariableTypes, "String")
	AddEnumItem(VariableTypes, "Boolean")
	AddEnumItem(VariableTypes, "Table")
	AddEnumItem(VariableTypes, "Closure")
	AddEnumItem(VariableTypes, "Nil")

	--> Additional Settings
	local AdditionalSettings = AddSubEnum("AdditionalSettings")
	AddEnumItem(AdditionalSettings, "ThrottleLoopInstructions")
	AddEnumItem(AdditionalSettings, "ThrottleRecursiveCalls")
	AddEnumItem(AdditionalSettings, "SandboxCalls")

	--> Script Global
	local ScriptGlobalType = AddSubEnum("ScriptGlobalType")
	AddEnumItem(ScriptGlobalType, "ServerScript")
	AddEnumItem(ScriptGlobalType, "Client")
	AddEnumItem(ScriptGlobalType, "None")
	AddEnumItem(ScriptGlobalType, "AutoResolve")
	
	RecursiveFreeze(EnumList, {"EnumType"})
	
	shared.SLVMEnums = EnumList
end

return {
	EnumList = EnumList,
	Utilities = Utilities
}
