import { CustomType as $CustomType } from "./gleam.mjs";
import { create_with_unique_name } from "./global_value_ffi.mjs";

export { create_with_unique_name };

class GleamGlobalValue extends $CustomType {}

class InvalidStoredFormat extends $CustomType {}

class DoesNotExist extends $CustomType {}
