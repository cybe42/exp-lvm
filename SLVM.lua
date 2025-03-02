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

local SLVM_DEBUG_ENABLED = false --> Enable to get VM logs

local SLVMConstructor, SLVM = {}, {}
local RunService = game:GetService("RunService")

local Types = loadstring(game.HttpService:GetAsync("https://github.com/cybe42/exp-lvm/raw/refs/heads/main/Types.lua"))()
local RuleConstructor = loadstring(game.HttpService:GetAsync("https://github.com/cybe42/exp-lvm/raw/refs/heads/main/Rule.lua"))()
local SLVMEnumModule = loadstring(game.HttpService:GetAsync("https://github.com/cybe42/exp-lvm/raw/refs/heads/main/Enum.lua"))()
local SLVMEnum, EnumUtilities = SLVMEnumModule.EnumList, SLVMEnumModule.Utilities

--// RLua Globals
local RLuaGlobals = {
	DockWidgetPluginGuiInfo = DockWidgetPluginGuiInfo, warn = warn, CFrame = CFrame,
	gcinfo = gcinfo, os = os, tick = tick, task = task, UDim = UDim, pairs = pairs,
	NumberSequence = NumberSequence, assert = assert, rawlen = rawlen, tonumber = tonumber,
	Color3 = Color3, Enum = Enum, Delay = Delay, OverlapParams = OverlapParams, Stats = Stats,
	_G = _G, RotationCurveKey = RotationCurveKey, coroutine = coroutine, NumberRange = NumberRange,
	FloatCurveKey = FloatCurveKey, PhysicalProperties = PhysicalProperties, Region3int16 = Region3int16,
	ypcall = ypcall, Font = Font, Ray = Ray, NumberSequenceKeypoint = NumberSequenceKeypoint, Version = Version,
	Vector2 = Vector2, version = version, Game = Game, delay = delay, spawn = spawn, stats = stats, string = string,
	wait = wait, UserSettings = UserSettings, settings = settings, _VERSION = _VERSION, loadstring = loadstring,
	printidentity = printidentity, CatalogSearchParams = CatalogSearchParams, UDim2 = UDim2, unpack = unpack, TweenInfo = TweenInfo,
	Wait = Wait, require = require, Vector3 = Vector3, Instance = Instance, Vector3int16 = Vector3int16, setmetatable = setmetatable,
	next = next, elapsedTime = elapsedTime, time = time, shared = shared, SharedTable = SharedTable, ipairs = ipairs, Workspace = Workspace, Faces = Faces,
	rawequal = rawequal, Vector2int16 = Vector2int16, collectgarbage = collectgarbage, game = game, newproxy = newproxy, Spawn = Spawn,
	DateTime = DateTime, Region3 = Region3, utf8 = utf8, xpcall = xpcall, Random = Random, rawset = rawset, PathWaypoint = PathWaypoint,
	tostring = tostring, RaycastParams = RaycastParams, workspace = workspace, typeof = typeof, math = math, bit32 = bit32, pcall = pcall,
	ColorSequenceKeypoint = ColorSequenceKeypoint, getfenv = getfenv, type = type, ColorSequence = ColorSequence, ElapsedTime = ElapsedTime,
	select = select, getmetatable = getmetatable, rawget = rawget, table = table, Rect =  Rect, BrickColor = BrickColor, setfenv = setfenv,
	debug = debug, Axes = Axes, error = error, print = print, buffer = buffer
}

--// Libraries
local RunInSLVM = loadstring(game.HttpService:GetAsync("https://github.com/cybe42/exp-lvm/raw/refs/heads/main/LuaVM.lua"))()

--// Constructor
function SLVMConstructor.new(ChunkName: string?, Env: {[any]: any}?): Types.SecureLuaVirtualMachine
	assert(type(ChunkName) == "string" or type(ChunkName) == "nil", "Expected string or nil as argument #1 for 'SLVM.new'")
	assert(type(Env) == "table" or type(Env) == "nil", "Expected table or nil as argument #2 for 'SLVM.new'")

	local self = setmetatable({
		LVMSettings = {
			ChunkName = ChunkName,
			Env = Env and setmetatable(Env, {
				__index = RLuaGlobals,
			}) or table.clone(RLuaGlobals),
		},

		ReplacedClosures = {},
		ReadHooks = {},
		WriteHooks = {};

		AdditionalSettings = {},
		Rules = {},

		MaxThreadCount = 20, --> Can be raised, but wouldn't recommend to

		CreatedInstances = {},
		CreatedThreads = {},

		ScriptGlobalType = SLVMEnum.ScriptGlobalType.AutoResolve,
	}, {
		__index = SLVM,
	})

	local OldInstNew; OldInstNew = self:ReplaceClosure(Instance.new, function(...)
		if type(...) == "string" and self:CheckCString("FloorWire", ...) then
			error("crash detection triggered - usage of deprecated instance 'FloorWire' which is not supported in SLVM")
		end

		local Success, Ret = pcall(OldInstNew, ...)
		if not Success then
			error(Ret, 2) --> Prevent xpcall attack
		end

		table.insert(self.CreatedInstances, Ret)
		return Ret
	end)

	local OldSharedNew; OldSharedNew = self:ReplaceClosure(SharedTable.new, function(...)
		local Args = {...}
		if type(Args[2]) == "table" and type(Args[3]) == "userdata" then
			error("crash detection triggered - please put the SharedTable's values in a different order", 2)
		end

		local Success, Ret = pcall(OldSharedNew, ...)
		if not Success then
			error(Ret, 2) --> Prevent xpcall attack
		end

		return Ret
	end)

	local OldSharedClone; OldSharedClone = self:ReplaceClosure(SharedTable.clone, function(...)
		local SharedTbl = ...
		if type(SharedTbl[2]) == "table" and type(SharedTbl[3]) == "userdata" then
			error("crash detection triggered - please put the SharedTable's values to clone in a different order", 2)
		end

		local Success, Ret = pcall(OldSharedClone, ...)
		if not Success then
			error(Ret, 2) --> Prevent xpcall attack
		end

		return Ret
	end)

	local CleaningThreads = false
	local OldTaskSpawn = task.spawn
	local TSpawnHook = function(...)
		if CleaningThreads then
			return
		elseif #self.CreatedThreads >= self.MaxThreadCount then
			CleaningThreads = true
			self:CloseCreatedThreads()
			task.delay(1, function()
				CleaningThreads = false
			end)
			
			error("too many threads - ending execution", 2)
		end

		local Success, Ret = pcall(OldTaskSpawn, ...)
		if not Success then
			error(Ret, 2) --> Prevent xpcall attack
		end

		table.insert(self.CreatedThreads, Ret)
		task.wait()
		
		return Ret
	end

	local TDelayHook = function(...)
		local DDelay, Closure = ...

		if type(DDelay) == "number" and type(Closure) == "function" then
			RunService.Heartbeat:Wait()
			task.wait(...)

			return TSpawnHook(Closure)
		end

		local Success, Ret = pcall(TSpawnHook, ...)
		if not Success then
			error(Ret, 2) --> Prevent xpcall attack
		end

		return Ret
	end

	--// That's what happens when roblox makes a thousand functions instead of making the deprecated have the same behavior as the new ones...
	self:ReplaceClosure(OldTaskSpawn, TSpawnHook)
	self:ReplaceClosure(spawn, TSpawnHook)
	self:ReplaceClosure(Spawn, TSpawnHook)

	self:ReplaceClosure(task.delay, TDelayHook)
	self:ReplaceClosure(delay, TDelayHook)
	self:ReplaceClosure(Delay, TDelayHook)

	self:AddRule(RuleConstructor.new(SLVMEnum.RuleType.TableRestriction, shared))

	local OldCoroutineCreate; OldCoroutineCreate = self:ReplaceClosure(coroutine.create, function(...)
		local Closure = ...
		return TSpawnHook(function()
			coroutine.yield()
			return Closure()
		end)
	end)

	local OldCoroutineWrap; OldCoroutineWrap = self:ReplaceClosure(coroutine.wrap, function(...)
		local Closure = ...
		return function()
			local Thread = TSpawnHook(function()
				coroutine.yield()
				return Closure()
			end)
			
			if Thread then
				coroutine.resume(Thread)
			end
		end
	end)

	local OldCoroutineClose; OldCoroutineClose = self:ReplaceClosure(coroutine.close, function(...)
		local Thread = ...
		local ThreadIndex = table.find(self.CreatedThreads, Thread)

		if coroutine.status(...) ~= "dead" then --> Is thread created by virtualized state
			local Success, Ret = pcall(OldCoroutineClose, ...)
			if not Success then
				error(Ret, 2)
			else
				table.remove(self.CreatedThreads, ThreadIndex)
			end
		end

		local Success, Ret = pcall(OldCoroutineClose, ...)
		if not Success then
			error(Ret, 2) --> Prevent xpcall attack
		end

		return Ret
	end)

	if buffer then --> Check if Buffer Luau Flag Enabled - Contact Liker if limits are needed to be raised in production
		local BufCreate; BufCreate = self:ReplaceClosure(buffer.create, function(...)
			if type(...) == "number" and ... >= 10000 then
				error("crash detection triggered - buffer size is bigger than 10000", 2)
			end

			local Success, Ret = pcall(BufCreate, ...)
			if not Success then
				error(Ret, 2) --> Prevent xpcall attack
			end

			return Ret
		end)

		local BufFromString; BufFromString = self:ReplaceClosure(buffer.fromstring, function(...)
			if type(...) == "number" and #(...) >= 10000 then
				error("crash detection triggered - string size is bigger than 10000", 2)
			end

			local Success, Ret = pcall(BufFromString, ...)
			if not Success then
				error(Ret, 2) --> Prevent xpcall attack
			end

			return Ret
		end)

		local BufFill; BufFill = self:ReplaceClosure(buffer.fill, function(...)
			local buf, offset, value, count = ... 

			if
				type(buf) == "buffer" and type(value) == "number"
				and offset >= 1e6 and value >= 4500000000
			then
				error("crash detection triggered - buffer fill with these arguments is an unsafe operation", 2)
			end

			local Success, Ret = pcall(BufFill, ...)
			if not Success then
				error(Ret, 2) --> Prevent xpcall attack
			end

			return Ret
		end)
	end

	self:EnableAdditionalSetting(SLVMEnum.AdditionalSettings.SandboxCalls) --> Now by default for better security - feel free to disable it
	return self
end

--// Lua Related Functions
function SLVM:Compile(Code: string): (Types.Closure?, string?)
	assert(type(Code) == "string", "Expected string as argument #1 for 'SLVM.Compile'")

	if not self:IsAdditionalSettingEnabled(SLVMEnum.AdditionalSettings.SandboxCalls) then
		warn("SLVM has detected that the additional setting `SandboxCalls` is disabled. It is HIGHLY recommended that you enable it. Only disable it for performance intensive tasks.")
	end

	local FinalSLVMRules = {
		SLVMTypes = {},
		SLVMBlock = {},

		SLVMReplaced = {},
		SLVMReadHook = {},
		SLVMWriteHook = {},

		AdditionalSettings = {},
	}

	local Env = self.LVMSettings.Env
	if table.find(self.AdditionalSettings, SLVMEnum.AdditionalSettings) then
		if self.ScriptGlobalType ~= SLVMEnum.ScriptGlobalType.None then
			Env.script = Instance.new(
				self.ScriptGlobalType == SLVMEnum.ScriptGlobalType.Server and "Script"
					or self.ScriptGlobalType == SLVMEnum.ScriptGlobalType.Client and "LocalScript"
					or self.ScriptGlobalType == SLVMEnum.ScriptGlobalType.AutoResolve and RunService:IsServer() and "Script" or "LocalScript"
			)
		end
	end

	FinalSLVMRules.AdvDebug = SLVM_DEBUG_ENABLED

	FinalSLVMRules.AdditionalSettings.ThrottleLoopInstructions = table.find(self.AdditionalSettings, SLVMEnum.AdditionalSettings.ThrottleLoopInstructions) and true or false
	FinalSLVMRules.AdditionalSettings.ThrottleRecursiveCalls = table.find(self.AdditionalSettings, SLVMEnum.AdditionalSettings.ThrottleRecursiveCalls) and true or false
	FinalSLVMRules.AdditionalSettings.SandboxCalls = table.find(self.AdditionalSettings, SLVMEnum.AdditionalSettings.SandboxCalls) and true or false

	--// Rules Handler
	for _, Rule: Types.Rule in pairs(self.Rules) do
		if Rule.Kind == SLVMEnum.RuleType.ClosureRestriction then
			FinalSLVMRules.SLVMBlock[Rule.Data] = Rule.Message or `The closure '{debug.info(Rule.Data, "n")}' has been blocked within this SLVM environment`
		elseif Rule.Kind == SLVMEnum.RuleType.UserDataRestriction then
			FinalSLVMRules.SLVMBlock[Rule.Data] = Rule.Message or `This userdata / instance has been blocked within this SLVM environment`
		elseif Rule.Kind == SLVMEnum.RuleType.TableRestriction then
			FinalSLVMRules.SLVMBlock[Rule.Data] = Rule.Message or `This table / library has been blocked within this SLVM environment`
		elseif Rule.Kind == SLVMEnum.RuleType.TypeRestriction then
			FinalSLVMRules.SLVMTypes[Rule.Data] = Rule.Message or `The variable type '{type(Rule.Data)}' has been blocked withing this SLVM encrionment`
		end
	end

	for Closure, NewClosure in pairs(self.ReplacedClosures) do
		FinalSLVMRules.SLVMReplaced[Closure] = NewClosure
	end

	for Self, ReadHook in pairs(self.ReadHooks) do
		FinalSLVMRules.SLVMReadHook[Self] = ReadHook
	end

	for Self, WriteHook in pairs(self.WriteHooks) do
		FinalSLVMRules.SLVMWriteHook[Self] = WriteHook
	end

	return RunInSLVM(Code, self.LVMSettings.ChunkName, Env, FinalSLVMRules)
end

function SLVM:Run(Code: string, ...: any?): ...any?
	assert(type(Code) == "string", "Expected string as argument #1 for 'SLVM.Run'")

	local Func, Error = self:Compile(Code)
	if not Func then
		error(Error, 2)
	end

	return Func(...)
end

--// Rules Manager
function SLVM:AddRule(Rule: Types.Rule): nil
	assert(type(Rule) == "table" and Rule.IsA, "Expected Rule as argument #1 for 'SLVM.AddRule'")

	table.insert(self.Rules, Rule)
end

function SLVM:RemoveRule(Rule: Types.Rule | number): nil
	assert((type(Rule) == "table" and Rule.IsA) or type(Rule) == "number", "Expected Rule or number as argument #1 for 'SLVM.RemoveRule'")

	if type(Rule) == "table" then
		local Index = table.find(self.Rules, Rule)

		if not Index then
			error("The rule isn't added to the SLVM rules", 2)
		end

		table.remove(self.Rules, Index)
	else
		if not self.Rules[Rule] then
			error("The rule at the given Index wasn't found", 2)
		end

		table.remove(self.Rules, Rule)
	end
end

--// Additional Settings Manager
function SLVM:IsAdditionalSettingEnabled(AdditionalSetting: Types.SLVMEnumItem): boolean
	local IsEnumItem = EnumUtilities:IsA(AdditionalSetting, "EnumItem")
	assert((type(AdditionalSetting) == "table" and AdditionalSetting.Name) or type(AdditionalSetting) == "string"
		or type(AdditionalSetting) == "number", "Expected SLVMEnumItem or string or number as argument #1 for 'SLVM.IsAdditionalSettingEnabled'")

	--// AutoResolver
	local AdditionalSettingKind: Types.SLVMEnumItem = AdditionalSetting;

	if not IsEnumItem then
		if type(AdditionalSetting) == "number" then
			AdditionalSettingKind = EnumUtilities:FindEnumItemByValue(SLVMEnum.RuleType, AdditionalSetting)

			if not AdditionalSetting then
				error("Provided EnumItem value does not exist for the expected SubEnum", 2)
			end
		elseif type(AdditionalSetting) == "string" then
			AdditionalSettingKind = EnumUtilities:FindEnumItemByName(SLVMEnum.RuleType, AdditionalSetting)

			if not AdditionalSetting then
				error("Provided EnumItem value does not exist for the expected SubEnum", 2)
			end
		end
	end

	return table.find(self.AdditionalSettings, AdditionalSettingKind) and true or false
end

function SLVM:EnableAdditionalSetting(AdditionalSetting: Types.SLVMEnumItem): nil
	local IsEnumItem = EnumUtilities:IsA(AdditionalSetting, "EnumItem")
	assert((type(AdditionalSetting) == "table" and AdditionalSetting.Name) or type(AdditionalSetting) == "string"
		or type(AdditionalSetting) == "number", "Expected SLVMEnumItem or string or number as argument #1 for 'SLVM.EnableAdditionalSetting'")

	--// AutoResolver
	local AdditionalSettingKind: Types.SLVMEnumItem = AdditionalSetting;

	if not IsEnumItem then
		if type(AdditionalSetting) == "number" then
			AdditionalSettingKind = EnumUtilities:FindEnumItemByValue(SLVMEnum.RuleType, AdditionalSetting)

			if not AdditionalSetting then
				error("Provided EnumItem value does not exist for the expected SubEnum", 2)
			end
		elseif type(AdditionalSetting) == "string" then
			AdditionalSettingKind = EnumUtilities:FindEnumItemByName(SLVMEnum.RuleType, AdditionalSetting)

			if not AdditionalSetting then
				error("Provided EnumItem value does not exist for the expected SubEnum", 2)
			end
		end
	end

	if table.find(self.AdditionalSettings, AdditionalSettingKind) then
		error("This additional setting is already enabled", 2)
	end

	table.insert(self.AdditionalSettings, AdditionalSettingKind)
end

function SLVM:DisableAdditionalSetting(AdditionalSetting: Types.SLVMEnumItem): nil
	local IsEnumItem = EnumUtilities:IsA(AdditionalSetting, "EnumItem")
	assert((type(AdditionalSetting) == "table" and AdditionalSetting.Name) or type(AdditionalSetting) == "string"
		or type(AdditionalSetting) == "number", "Expected SLVMEnumItem or string or number as argument #1 for 'SLVM.DisableAdditionalSetting'")

	--// AutoResolver
	local AdditionalSettingKind: Types.SLVMEnumItem = AdditionalSetting;

	if not IsEnumItem then
		if type(AdditionalSetting) == "number" then
			AdditionalSettingKind = EnumUtilities:FindEnumItemByValue(SLVMEnum.RuleType, AdditionalSetting)

			if not AdditionalSetting then
				error("Provided EnumItem value does not exist for the expected SubEnum", 2)
			end
		elseif type(AdditionalSetting) == "string" then
			AdditionalSettingKind = EnumUtilities:FindEnumItemByName(SLVMEnum.RuleType, AdditionalSetting)

			if not AdditionalSetting then
				error("Provided EnumItem value does not exist for the expected SubEnum", 2)
			end
		end
	end

	local Index = table.find(self.AdditionalSettings, AdditionalSettingKind)
	if not Index then
		error("This additional setting is not enabled", 2)
	end

	table.remove(self.AdditionalSettings, Index)
end

--// Misc
function SLVM:ReplaceClosure(Closure: Closure, NewClosure: Closure): Closure
	assert(type(Closure) == "function", "Expected function as argument #1 for 'SLVM.ReplaceClosure'")
	assert(type(NewClosure) == "function", "Expected function as argument #2 for 'SLVM.ReplaceClosure'")

	local OldClosure = self.ReplacedClosures[Closure] or Closure
	self.ReplacedClosures[Closure] = NewClosure

	return OldClosure
end

function SLVM:AddReadHook(Object: {any?} | Instance, Hook: Closure): nil
	assert(type(Object) == "table" or typeof(Object) == "Instance", "Expected table or Instance as argument #1 for 'SLVM.AddReadHook'")
	assert(type(Hook) == "function", "Expected function as argument #2 for 'SLVM.AddReadHook'")

	if typeof(Object) == "Instance" then
		Object = "InstanceHook"
	end

	self.ReadHooks[Object] = Hook
end

function SLVM:AddWriteHook(Object: {any?} | Instance, Hook: Closure): nil
	assert(type(Object) == "table" or typeof(Object) == "Instance", "Expected table or Instance as argument #1 for 'SLVM.AddWriteHook'")
	assert(type(Hook) == "function", "Expected function as argument #2 for 'SLVM.AddWriteHook'")

	if typeof(Object) == "Instance" then
		Object = "InstanceHook"
	end

	self.WriteHooks[Object] = Hook
end

function SLVM:CheckCString(String: string, CString: string): boolean
	assert(type(String) == "string", "Expected string as argument #1 for 'SLVM.CheckCString'")
	assert(type(CString) == "string", "Expected string as argument #2 for 'SLVM.CheckCString'")

	return String == string.gsub(CString, "%z.*", "")
end

function SLVM:ChangeChunkName(NewChunkName: string): nil
	assert(type(NewChunkName) == "string", "Expected string as argument #1 for 'SLVM.ChangeChunkName'")

	self.LVMSettings.ChunkName = NewChunkName
end

function SLVM:DeleteCreatedInstances(Callback: (Instance, ...any) -> boolean?): nil
	assert(type(Callback) == "function" or type(Callback) == "nil", "Expected function or nil as argument #1 for 'SLVM.DeleteCreatedInstances'")

	for Index, CreatedInstance: Instance in pairs(self.CreatedInstances) do
		if (not Callback) or Callback(CreatedInstance) then
			CreatedInstance:Destroy()

			self.CreatedInstances[Index] = nil
		end
	end
end

function SLVM:CloseCreatedThreads(Callback: (thread, ...any) -> boolean?): nil
	assert(type(Callback) == "function" or type(Callback) == "nil", "Expected function or nil as argument #1 for 'SLVM.CloseCreatedThreads'")

	for Index, CreatedThread: thread in pairs(self.CreatedThreads) do
		if (not Callback) or Callback(CreatedThread) then
			pcall(coroutine.close, CreatedThread)
			self.CreatedThreads[Index] = nil
		end
	end
end

return {
	LuaVM = SLVMConstructor,
	Enum = SLVMEnum,
	Rule = RuleConstructor,
}
