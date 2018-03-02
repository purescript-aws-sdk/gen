# Gen

Generates the purescript-aws-sdk modules from the [AWS SDK JS](https://github.com/aws/aws-sdk-js).

## Getting started

```sh
make clean init build run
```

## Warning: `CycleInDeclaration`

AWS SDK JS has a few types with properties referencing themselves. As a quick fix, all properties that would raise
`CycleInDeclaration` aren't in the generated code. Bellow are the missing properties:

  - `Or` in `AWS.CostExplorer.Expression`
  - `And` in `AWS.CostExplorer.Expression`
  - `Not` in `AWS.CostExplorer.Expression`
  - `L` in `AWS.DynamoDBStreams.AttributeValue`
  - `M` in `AWS.DynamoDBStreams.AttributeValue`
  - `Configurations` in `AWS.EMR.Configuration`
  - `Resources` in `AWS.Organizations.HandshakeResource`
  - `Aggregators` in `AWS.SSM.InventoryAggregator`
