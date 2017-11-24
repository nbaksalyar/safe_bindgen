public class FfiException : Exception {
    private int _code;

    public int Code {
        get { return _code; }
    }

    internal FfiException(FfiResult result)
        : base(result.error)
    {
        _code = result.errorCode;
    }
}

[StructLayout(LayoutKind.Sequential)]
internal class FfiResult {
    public int errorCode;
    [MarshalAs(UnmanagedType.LPStr)]
    public String error;
}

internal class Utilities {
    public static IntPtr ToHandlePtr<T>(T obj) {
        return GCHandle.ToIntPtr(GCHandle.Alloc(obj));
    }

    public static T FromHandlePtr<T>(IntPtr ptr, bool free = true) {
        var handle = GCHandle.FromIntPtr(ptr);
        var result = (T) handle.Target;

        if (free) handle.Free();

        return result;
    }

    public static (Task<T>, IntPtr) PrepareTask<T>() {
        var tcs = new TaskCompletionSource<T>();
        var userData = ToHandlePtr(tcs);

        return (tcs.Task, userData);
    }

    public static (Task, IntPtr) PrepareTask() {
        return PrepareTask<bool>();
    }

    public static void CompleteTask<T>(TaskCompletionSource<T> tcs, FfiResult result, T arg) {
        if (result.errorCode != 0) {
            tcs.SetException(new FfiException(result));
        } else {
            tcs.SetResult(arg);
        }
    }

    public static void CompleteTask<T>(IntPtr userData, FfiResult result, T arg) {
        var tcs = FromHandlePtr<TaskCompletionSource<T>>(userData);
        CompleteTask(tcs, result, arg);
    }

    public static void CompleteTask(IntPtr userData, FfiResult result) {
        CompleteTask(userData, result, true);
    }
}
