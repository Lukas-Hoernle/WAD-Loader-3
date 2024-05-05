package com.luma.wadloader3;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.ConfigurationPropertiesScan;

@SpringBootApplication
@ConfigurationPropertiesScan
public class Wadloader3Application {

	public static void main(String[] args) {
		SpringApplication.run(Wadloader3Application.class, args);
	}

}
