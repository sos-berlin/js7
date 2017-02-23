package com.sos.scheduler.engine.taskserver.dotnet;

import java.util.Optional;

import system.Type;
import system.Object;
import system.reflection.ConstructorInfo;
import system.reflection.MethodInfo;

public class DotnetInvoker {

	public static Object createInstance(Type type, Type[] paramTypes, Object[] params) {
		ConstructorInfo ci = Optional
				.ofNullable(type.GetConstructor(paramTypes))
				.orElseThrow(
						() -> new RuntimeException(
								String.format(
										"[%s] Can't find the constructor with the specified parameters",
										type.getAssembly().getLocation())));
		return ci.Invoke(params);
	}

	public static system.Object invokeMethod(Type type, Object instance,
			String methodName, Type[] paramTypes, Object[] params) {
		MethodInfo mi = Optional
				.ofNullable(type.GetMethod(methodName, paramTypes))
				.orElseThrow(
						() -> new RuntimeException(
								String.format(
										"[%s] Can't find the method %s with the specified parameters",
										type.getAssembly().getLocation(),
										methodName)));

		return mi.Invoke(instance, params);
	}

	public static system.Object invokeMethod(Type type, Object instance, String methodName, String value) {
		MethodInfo mi = Optional
				.ofNullable(
						type.GetMethod(methodName,
								new system.Type[] { system.Type
										.GetType("System.String") }))
				.orElseThrow(
						() -> new RuntimeException(
								String.format(
										"[%s] Can't find the method %s with the specified string parameter %s",
										type.getAssembly().getLocation(),
										methodName, value)));

		return mi.Invoke(instance, new system.Object[] { new system.String(
				value) });
	}

	public static system.Object invokeMethod(Type type, Object instance, String methodName) {
		MethodInfo mi = Optional.ofNullable(
				type.GetMethod(methodName, new system.Type[] {})).orElseThrow(
				() -> new RuntimeException(String.format(
						"[%s] Can't find the method %s", type.getAssembly()
								.getLocation(), methodName)));

		return mi.Invoke(instance, null);
	}

	public static boolean invokeMethod(Type type, Object instance,
			String methodName, boolean defaultValue) {
		return getReturnValue(invokeMethod(type, instance, methodName),
				defaultValue);
	}

	public static boolean invokeMethod(Type type, Object instance,
			String methodName, Type[] paramTypes, Object[] params,
			boolean defaultValue) {
		return getReturnValue(
				invokeMethod(type, instance, methodName, paramTypes, params),
				defaultValue);
	}

	public static boolean getReturnValue(system.Object obj, boolean defaultValue) {
		if (obj.GetType().getName().equalsIgnoreCase("boolean")) {
			return Boolean.parseBoolean(obj.toString());
		}
		return defaultValue;
	}
}
