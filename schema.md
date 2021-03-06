# Database Schema

We have the following tables.

## Ingredients

A table containing all known raw ingredients.

### Fields

| Field name   | Type      | Constraints |
| ------------ | --------- | ----------- |
| IngredientId | BIGSERIAL | Primary Key |
| Name         | TEXT      | NOT NULL    |

## Recipes

A table containing all recipes.

### Fields

| Field name   | Type      | Constraints                            |
| ------------ | --------- | -------------------------------------- |
| RecipeId     | BIGSERIAL | Primary Key                            |
| Name         | TEXT      | NOT NULL                               |
| Components   | BIGINT[]  | NOT NULL ELEMENT REFERENCES Components |
| Instructions | TEXT      | NOT NULL                               |

## Components

All components of any recipe. A component can either be a raw
ingredient or a recipe.

### Fields

| Field name         | Type             | Constraints |
| ------------------ | ---------------- | ----------- |
| ComponentId        | BIGSERIAL        | PRIMARY KEY |
| IsIngredient       | Boolean          | NOT NULL    |
| IngredientOrRecipe | BIGINT           |             |
| Quantity           | DOUBLE PRECISION | NOT NULL    |
| Unit               | TEXT             |             |
