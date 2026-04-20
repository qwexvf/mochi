-record(telemetry_config, {
    enabled :: boolean(),
    track_fields :: boolean(),
    track_dataloaders :: boolean(),
    handler :: fun((mochi@telemetry:telemetry_event()) -> nil)
}).
