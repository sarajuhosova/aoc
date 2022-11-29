package aoc2020.day21;

import com.google.common.collect.Sets;
import library.Year;
import library.io.Input;

import java.util.*;
import java.util.stream.Collectors;

public class FoodAllergies {

    static class Food {

        Set<String> ingredients;
        Set<String> allergens;

        public Food() {
            this.ingredients = new HashSet<>();
            this.allergens = new HashSet<>();
        }

        public void addIngredient(String string) {
            ingredients.add(string);
        }

        public void addAllergens(String string) {
            allergens.add(string);
        }
    }

    public static List<Food> parse(List<String> data) {
        List<Food> foods = new ArrayList<>();

        for (String s : data) {
            String[] split = s.split(" [(]contains |[)]");
            Food f = new Food();
            for (String i : split[0].split(" ")) f.addIngredient(i);
            for (String a : split[1].split(", ")) f.addAllergens(a);
            foods.add(f);
        }

        return foods;
    }

    private static Map<String, String> matchAllergens(List<Food> foods) {
        Map<String, Set<String>> map = foods.stream()
                .flatMap(f -> f.allergens.stream())
                .distinct()
                .collect(Collectors.toMap(a -> a, a -> new HashSet<>()));

        for (String allergen : map.keySet()) {
            Set<String> ingredients = foods.stream()
                    .flatMap(f -> f.ingredients.stream())
                    .collect(Collectors.toSet());
            for (Food f : foods) {
                if (!f.allergens.contains(allergen)) continue;
                ingredients = Sets.intersection(ingredients, f.ingredients);
            }
            map.put(allergen, ingredients);
        }

        Map<String, String> answer = new HashMap<>();

        while (!map.isEmpty()) {
            for (String allergen : map.keySet()) {
                Set<String> ingredients = map.get(allergen);

                if (ingredients.size() != 1) continue;
                answer.put(allergen, ingredients.stream().findAny().get());
            }

            for (String allergen : answer.keySet()) map.remove(allergen);

            for (String allergen : map.keySet()) {
                map.put(allergen, Sets.difference(map.get(allergen), new HashSet<>(answer.values())));
            }
        }

        return answer;
    }

    public static long part1(Map<String, String> map, List<Food> foods) {
        return foods.stream()
                .flatMap(f -> f.ingredients.stream())
                .filter(i -> !map.containsValue(i))
                .count();
    }

    public static String part2(Map<String, String> map) {
        return map.keySet().stream()
                .sorted()
                .map(map::get)
                .collect(Collectors.joining(","));
    }

    public static void main(String[] args) {
        List<String> data = Input.readData(Year._2020, "day21.txt").collect(Collectors.toList());
        List<Food> foods = parse(data);

        Map<String, String> map = matchAllergens(foods);

        System.out.println(part1(map, foods));
        System.out.println(part2(map));
    }

}
